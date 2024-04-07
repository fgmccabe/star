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

ReturnStatus g__fork(heapPo h, termPo a1) {
  labelPo fn = C_LBL(a1);
  processPo np = newProcess(h, labelCode(fn), currentProcess->wd, unitEnum);

  threadPo thread = newThread(np, globalHeap);

  pthread_attr_t detach;

  if (pthread_attr_init(&detach) != 0)
    syserr("cannot initiate attributes object");
  else if (pthread_attr_setdetachstate(&detach, PTHREAD_CREATE_JOINABLE) != 0)
    syserr("cannot set create-detached");
  if (pthread_create(&np->threadID, &detach, forkThread, np) != 0)
    syserr("cannot fork a thread");

  return (ReturnStatus) {.ret=Normal, .result=(termPo) thread};
}

ReturnStatus g__kill(heapPo h, termPo xc, termPo a1) {
  threadPo th = C_THREAD(a1);

  processPo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != currentProcess) {
    ps_kill(tgt);
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eINVAL};
}

ReturnStatus g__thread(heapPo h) {
  return (ReturnStatus) {.ret=Normal, .result=(termPo) currentProcess->thread};
}

ReturnStatus g__thread_state(heapPo h, termPo a1) {
  threadPo th = C_THREAD(a1);
  processPo tgt = getThreadProcess(th);

  switchProcessState(currentProcess, in_exclusion);

  termPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead], dead, globalHeap);
  else
    st = declareEnum(state_names[tgt->state], dead, globalHeap);
  setProcessRunnable(currentProcess);
  return (ReturnStatus) {.ret=Normal, .result=st};
}

ReturnStatus g__waitfor(heapPo h, termPo xc, termPo a1) {
  threadPo th = C_THREAD(a1);
  processPo tgt = getThreadProcess(th);

  if (tgt == NULL) {
    return (ReturnStatus) {.ret=Normal, .result=(termPo) unitEnum};
  } else if (tgt != currentProcess) {
    pthread_t thread = tgt->threadID;
    void *result;      /* This is ignored */

    switchProcessState(currentProcess, wait_term);
    if (pthread_join(thread, &result) == 0) {
      setProcessRunnable(currentProcess);
      return (ReturnStatus) {.ret=Normal, .result=(termPo) unitEnum};
    } else {
      setProcessRunnable(currentProcess);
      switch (errno) {
        case EINVAL:
          return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eINVAL};
        case ESRCH:
          return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eNOTFND};
        case EDEADLK:
          return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eDEAD};
        default: {
          return (ReturnStatus) {.ret=Normal, .result=(termPo) voidEnum};
        }
      }
    }
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eDEAD};
}

ReturnStatus g__abort(heapPo h, termPo lc, termPo msg) {
  logMsg(logFile, "Abort %T at %L", msg, lc);
  verifyProc(currentProcess, h);
  stackTrace(currentProcess, logFile, currentProcess->stk, displayDepth, showPrognames);
  star_exit(99);
  return (ReturnStatus) {.ret=Normal, .result=(termPo) voidEnum};
}

ReturnStatus g__stackTrace(heapPo h) {
  strBufferPo str = newStringBuffer();

  stackTrace(currentProcess, O_IO(str), currentProcess->stk, displayDepth, showPrognames);

  ReturnStatus rt = {.ret=Normal, .result=allocateFromStrBuffer(h, str)};
  closeIo(O_IO(str));

  return rt;
}
