//
// Created by Francis McCabe on 3/11/18.
//


#include <errorCodes.h>
#include <labels.h>
#include <errno.h>
#include "threds.h"
#include <globals.h>
#include "debug.h"

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
//  pthread_cleanup_push(destroyThread, P);

//    runGo(P);        // start the execution of L&O code

//  pthread_cleanup_pop(True);
  return NULL;
}

ReturnStatus g__fork(processPo P, ptrPo tos) {
  labelPo fn = C_LBL(tos[0]);
  processPo np = newProcess(labelCode(fn));

  threadPo thread = newThread(np);

  pthread_attr_t detach;

  if (pthread_attr_init(&detach) != 0)
    syserr("cannot initiate attributes object");
  else if (pthread_attr_setdetachstate(&detach, PTHREAD_CREATE_JOINABLE) != 0)
    syserr("cannot set create-detached");
  if (pthread_create(&np->threadID, &detach, forkThread, np) != 0)
    syserr("cannot fork a thread");

  return (ReturnStatus) {.ret=Ok, .result=(termPo) thread};
}

ReturnStatus g__kill(processPo P, ptrPo tos) {
  threadPo th = C_THREAD(tos[0]);

  processPo tgt = getThreadProcess(th);

  if (tgt != NULL && tgt != P) {
    ps_kill(tgt);
    return (ReturnStatus) {.ret=Ok, .result=voidEnum};
  } else
    return liberror(P, "kill", eINVAL);
}

ReturnStatus g__thread(processPo P, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) P->thread};
}

ReturnStatus g__thread_state(processPo P, ptrPo tos) {
  threadPo th = C_THREAD(tos[0]);
  processPo tgt = getThreadProcess(th);

  switchProcessState(P, in_exclusion);

  labelPo st;

  if (tgt == NULL)
    st = declareEnum(state_names[dead]);
  else
    st = declareEnum(state_names[tgt->state]);
  setProcessRunnable(P);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) st};
}

ReturnStatus g__waitfor(processPo P, ptrPo tos) {
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
          return liberror(P, "_waitfor", eINVAL);
        case ESRCH:
          return liberror(P, "_waitfor", eNOTFND);
        case EDEADLK:
          return liberror(P, "_waitfor", eDEAD);
        default: {
          return (ReturnStatus) {.ret=Ok, .result=(termPo) voidEnum};
        }
      }
    }
  } else
    return liberror(P, "_waitfor", eDEAD);
}

ReturnStatus g__abort(processPo P, ptrPo tos) {
  termPo lc = tos[0];
  termPo msg = tos[1];

  logMsg(logFile, "Abort %T at %L", msg, lc);
  dumpStackTrace(P,logFile);

  return (ReturnStatus) {.ret=Error, .result=(termPo) voidEnum};
}

ReturnStatus g__stackTrace(processPo P, ptrPo tos) {
  stackTrace(P, logFile, False);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) voidEnum};
}
