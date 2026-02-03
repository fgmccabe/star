//
// Created by Francis McCabe on 3/11/18.
//

#include <errorCodes.h>
#include <labels.h>
#include <errno.h>
#include "threds.h"
#include "globals.h"
#include "arith.h"

static const char *state_names[] = {
  "star.thread#quiescent",
  "star.thread#runnable",
  "star.thread#wait_io",
  "star.thread#wait_timer",
  "star.thread#wait_term",
  "star.thread#wait_lock",
  "star.thread#wait_child",
  "star.thread#wait_rendezvous",
  "star.thread#in_exclusion",
  "star.thread#dead"
};

void *forkThread(void *arg) {
  enginePo P = (enginePo) arg;

  pthread_setspecific(processKey, P);
  P->state = runnable;
  pthread_cleanup_push((void (*)(void *)) ps_kill, P);
    run(P); // start the execution of star code
  pthread_cleanup_pop(True);
  return NULL;
}

ValueReturn s__fork(enginePo P, termPo l) {
  labelPo fn = C_LBL(l);
  heapPo h = processHeap(P);
#ifndef NOJIT
  enginePo np = newEngine(h, jitOnLoad, labelMtd(fn), P->wd, unitEnum);
#else
  enginePo np = newEngine(h, False, labelMtd(fn), P->wd, unitEnum);
#endif
  threadPo thread = newThread(np, globalHeap);

  pthread_attr_t detach;

  if (pthread_attr_init(&detach) != 0)
    syserr("cannot initiate attributes object");
  else if (pthread_attr_setdetachstate(&detach, PTHREAD_CREATE_JOINABLE) != 0)
    syserr("cannot set create-detached");
  if (pthread_create(&np->threadID, &detach, forkThread, np) != 0)
    syserr("cannot fork a thread");

  return normalReturn((termPo)thread);
}

ReturnStatus g__fork(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__fork(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__kill(enginePo P, termPo t) {
  threadPo th = C_THREAD(t);

  enginePo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != P) {
    ps_kill(tgt);
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__kill(enginePo P) {
  termPo t = popVal(P);
  ValueReturn ret = s__kill(P, t);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__thread(enginePo P) {
  return normalReturn((termPo)P->thread);
}

ReturnStatus g__thread(enginePo P) {
  pshVal(P, (termPo) P->thread);
  return Normal;
}

ValueReturn s__thread_state(enginePo P, termPo t) {
  threadPo th = C_THREAD(t);
  enginePo tgt = getThreadProcess(th);

  switchProcessState(P, in_exclusion);

  termPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead], dead, globalHeap);
  else
    st = declareEnum(state_names[tgt->state], tgt->state, globalHeap);
  setProcessRunnable(P);
  return normalReturn(st);
}

ReturnStatus g__thread_state(enginePo P) {
  termPo t = popVal(P);
  ValueReturn ret = s__thread_state(P, t);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__waitfor(enginePo P, termPo t) {
  threadPo th = C_THREAD(t);
  enginePo tgt = getThreadProcess(th);

  if (tgt == NULL) {
    return normalReturn(unitEnum);
  } else if (tgt != P) {
    pthread_t thread = tgt->threadID;
    void *result; /* This is ignored */

    switchProcessState(P, wait_term);
    if (pthread_join(thread, &result) == 0) {
      setProcessRunnable(P);
      return normalReturn(unitEnum);
    } else {
      setProcessRunnable(P);
      switch (errno) {
        case EINVAL:
          return abnormalReturn(eINVAL);
        case ESRCH:
          return abnormalReturn(eNOTFND);
        case EDEADLK:
          return abnormalReturn(eDEAD);
        default: {
          return normalReturn(unitEnum);
        }
      }
    }
  } else {
    return abnormalReturn(eDEAD);
  }
}

ReturnStatus g__waitfor(enginePo P) {
  termPo t = popVal(P);
  ValueReturn ret = s__waitfor(P, t);
  pshVal(P, ret.value);
  return ret.status;
}
