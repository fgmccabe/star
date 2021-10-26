//
// Created by Francis McCabe on 3/11/18.
//


#include <errorCodes.h>
#include <labels.h>
#include <errno.h>
#include "threds.h"
#include <globals.h>
#include <strings.h>
#include "debugP.h"

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

ReturnStatus g__fork(processPo P, heapPo h, ptrPo tos) {
  labelPo fn = C_LBL(tos[0]);
  processPo np = newProcess(h, labelCode(fn), P->wd, processCap(P), unitEnum);

  threadPo thread = newThread(np, globalHeap);

  pthread_attr_t detach;

  if (pthread_attr_init(&detach) != 0)
    syserr("cannot initiate attributes object");
  else if (pthread_attr_setdetachstate(&detach, PTHREAD_CREATE_JOINABLE) != 0)
    syserr("cannot set create-detached");
  if (pthread_create(&np->threadID, &detach, forkThread, np) != 0)
    syserr("cannot fork a thread");

  return (ReturnStatus) {.ret=Ok, .result=(termPo) thread};
}

ReturnStatus g__kill(processPo P, heapPo h, ptrPo tos) {
  threadPo th = C_THREAD(tos[0]);

  processPo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != P) {
    ps_kill(tgt);
    return (ReturnStatus) {.ret=Ok, .result=voidEnum};
  } else
    return liberror(P, h, "kill", eINVAL);
}

ReturnStatus g__thread(processPo P, heapPo h, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) P->thread};
}

ReturnStatus g__thread_state(processPo P, heapPo h, ptrPo tos) {
  threadPo th = C_THREAD(tos[0]);
  processPo tgt = getThreadProcess(th);

  switchProcessState(P, in_exclusion);

  termPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead], dead, globalHeap);
  else
    st = declareEnum(state_names[tgt->state], dead, globalHeap);
  setProcessRunnable(P);
  return (ReturnStatus) {.ret=Ok, .result=st};
}

ReturnStatus g__waitfor(processPo P, heapPo h, ptrPo tos) {
  threadPo th = C_THREAD(tos[0]);
  processPo tgt = getThreadProcess(th);

  if (tgt == NULL) {
    ReturnStatus rt = {.ret=Ok, .result=(termPo) voidEnum};
    return rt;
  } else if (tgt != P) {
    pthread_t thread = tgt->threadID;
    void *result;      /* This is ignored */

    switchProcessState(P, wait_term);
    if (pthread_join(thread, &result) == 0) {
      setProcessRunnable(P);
      return (ReturnStatus) {.ret=Ok, .result=(termPo) voidEnum};
    } else {
      setProcessRunnable(P);
      switch (errno) {
        case EINVAL:
          return liberror(P, h, "_waitfor", eINVAL);
        case ESRCH:
          return liberror(P, h, "_waitfor", eNOTFND);
        case EDEADLK:
          return liberror(P, h, "_waitfor", eDEAD);
        default: {
          return (ReturnStatus) {.ret=Ok, .result=(termPo) voidEnum};
        }
      }
    }
  } else
    return liberror(P, h, "_waitfor", eDEAD);
}

ReturnStatus g__abort(processPo P, heapPo h, ptrPo tos) {
  termPo lc = tos[0];
  termPo msg = tos[1];

  logMsg(logFile, "Abort %T at %L", msg, lc);
  verifyProc(P, h);
  stackTrace(P, NULL, logFile, P->stk, True);

  return (ReturnStatus) {.ret=Error, .result=(termPo) voidEnum};
}

ReturnStatus g__stackTrace(processPo P, heapPo h, ptrPo tos) {
  strBufferPo str = newStringBuffer();

  stackTrace(P, NULL, O_IO(str), P->stk, False);

  ReturnStatus rt = {.ret=Ok, .result=allocateFromStrBuffer(str, h)};
  closeFile(O_IO(str));

  return rt;
}
