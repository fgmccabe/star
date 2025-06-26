//
// Created by Francis McCabe on 3/11/18.
//


#include <errorCodes.h>
#include <labels.h>
#include <errno.h>
#include "threds.h"
#include "globals.h"
#include "arith.h"

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
  enginePo P = (enginePo) arg;

  pthread_setspecific(processKey, P);
  P->state = runnable;
  pthread_cleanup_push((void (*)(void *)) ps_kill, P);
    run(P);        // start the execution of star code
  pthread_cleanup_pop(True);
  return NULL;
}

ReturnStatus g__fork(enginePo P) {
  labelPo fn = C_LBL(popVal(P));
  heapPo h = processHeap(P);
  enginePo np = newProcess(h, labelCode(fn), P->wd, unitEnum);

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

ReturnStatus g__kill(enginePo P) {
  threadPo th = C_THREAD(popVal(P));

  enginePo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != P) {
    ps_kill(tgt);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__thread(enginePo P) {
  pshVal(P, (termPo) P->thread);
  return Normal;
}

ReturnStatus g__thread_state(enginePo P) {
  threadPo th = C_THREAD(popVal(P));
  enginePo tgt = getThreadProcess(th);

  switchProcessState(P, in_exclusion);

  termPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead], dead, globalHeap);
  else
    st = declareEnum(state_names[tgt->state], tgt->state, globalHeap);
  setProcessRunnable(P);
  pshVal(P, st);
  return Normal;
}

ReturnStatus g__waitfor(enginePo P) {
  threadPo th = C_THREAD(popVal(P));
  enginePo tgt = getThreadProcess(th);

  if (tgt == NULL) {
    pshVal(P,unitEnum);
    return Normal;
  } else if (tgt != P) {
    pthread_t thread = tgt->threadID;
    void *result;      /* This is ignored */

    switchProcessState(P, wait_term);
    if (pthread_join(thread, &result) == 0) {
      setProcessRunnable(P);
      pshVal(P, unitEnum);
      return Normal;
    } else {
      setProcessRunnable(P);
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

