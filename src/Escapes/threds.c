//
// Created by Francis McCabe on 3/11/18.
//


#include <errorCodes.h>
#include <labels.h>
#include <errno.h>
#include "threds.h"
#include <globals.h>

static const char *state_names[] = {"star.thread#quiescent",
                                    "star.thread#runnable",
                                    "star.thread#wait_io",
                                    "star.thread#wait_timer",
                                    "star.thread#wait_term",
                                    "star.thread#wait_lock",
                                    "star.thread#wait_child",
                                    "star.thread#wait_rendezvous",
                                    "star.thread#in_exclusion",
                                    "star.thread#dead"};

void *forkThread(void *arg) {
  processPo P = (processPo) arg;

  pthread_setspecific(processKey, P);
  P->state = runnable;
  pthread_cleanup_push((void (*)(void *)) ps_kill, P);
    run(P);        // start the execution of star code
  pthread_cleanup_pop(True);
  return NULL;
}

ReturnStatus g__fork(processPo P) {
  labelPo fn = C_LBL(popVal(P));
  processPo np = newProcess(currentHeap, labelCode(fn), currentProcess->wd, unitEnum);

  threadPo thread = newThread(np, globalHeap);

  pthread_attr_t detach;

  if (pthread_attr_init(&detach) != 0)
    syserr("cannot initiate attributes object");
  else if (pthread_attr_setdetachstate(&detach, PTHREAD_CREATE_JOINABLE) != 0)
    syserr("cannot set create-detached");
  if (pthread_create(&np->threadID, &detach, forkThread, np) != 0)
    syserr("cannot fork a thread");

  pshVal(P, (termPo) thread);
  return Normal;
}

ReturnStatus g__kill(processPo P) {
  threadPo th = C_THREAD(popVal(P));

  processPo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != currentProcess) {
    ps_kill(tgt);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__thread(processPo P) {
  pshVal(P, (termPo) currentProcess->thread);
  return Normal;
}

ReturnStatus g__thread_state(processPo P) {
  threadPo th = C_THREAD(popVal(P));
  processPo tgt = getThreadProcess(th);

  switchProcessState(currentProcess, in_exclusion);

  termPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead], dead, globalHeap);
  else
    st = declareEnum(state_names[tgt->state], tgt->state, globalHeap);
  setProcessRunnable(currentProcess);
  pshVal(P, st);
  return Normal;
}

ReturnStatus g__waitfor(processPo P) {
  threadPo th = C_THREAD(popVal(P));
  processPo tgt = getThreadProcess(th);

  if (tgt == NULL) {
    pshVal(P,unitEnum);
    return Normal;
  } else if (tgt != currentProcess) {
    pthread_t thread = tgt->threadID;
    void *result;      /* This is ignored */

    switchProcessState(currentProcess, wait_term);
    if (pthread_join(thread, &result) == 0) {
      setProcessRunnable(currentProcess);
      pshVal(P, unitEnum);
      return Normal;
    } else {
      setProcessRunnable(currentProcess);
      switch (errno) {
        case EINVAL:
          pshVal(P, eINVAL);
          return Abnormal;
        case ESRCH:
          pshVal(P, eNOTFND);
          return Abnormal;
        case EDEADLK:
          pshVal(P, eDEAD);
          return Abnormal;
        default: {
          pshVal(P, unitEnum);
          return Normal;
        }
      }
    }
  } else {
    pshVal(P, eDEAD);
    return Abnormal;
  }
}

ReturnStatus g__abort(processPo P) {
  termPo lc = popVal(P);
  termPo msg = popVal(P);
  logMsg(logFile, "Abort %T at %L", msg, lc);
  verifyProc(currentProcess, currentHeap);
  stackTrace(currentProcess, logFile, currentProcess->stk, displayDepth, showPrognames, -1);
  star_exit(99);
  pshVal(P, unitEnum);
  return Normal;
}

ReturnStatus g__stackTrace(processPo P) {
  strBufferPo str = newStringBuffer();

  stackTrace(currentProcess, O_IO(str), currentProcess->stk, displayDepth, showArguments, -1);

  pshVal(P, allocateFromStrBuffer(currentHeap, str));
  closeIo(O_IO(str));

  return Normal;
}
