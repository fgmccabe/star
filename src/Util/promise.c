//
// Created by Francis McCabe on 8/11/21.
//

#include "promiseP.h"
#include <stdlib.h>
#include <assert.h>

static void initPromiseClass(classPo class, classPo request);
static void promiseInit(objectPo o, va_list *args);

PromiseClassRec PromiseClass = {
  {
    (classPo) &LockedClass,               /* parent class is object */
    "promise",                            /* this is the promise class */
    initPromiseClass,                     /* Promise class initializer */
    O_INHERIT_DEF,                        /* Promise object element creation */
    O_INHERIT_DEF,                        /* Promise objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    promiseInit,                          /* initialization of a Promise buffer */
    sizeof(PromiseObject),                /* min size of a Promise record -- should never use */
    NULL,                       /* pool of values for this class */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,                    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},                         // Nothing in the lockable class
  {
    Null                         // Empty active queue
  }
};

classPo promiseClass = (classPo) &PromiseClass;

static pthread_once_t promiseOnce = PTHREAD_ONCE_INIT;

static void initPromiseEtc(void) {
  PromiseClass.objectPart.parent = lockedClass;
  initRecursiveMutex(&promiseClass->mutex);
}

void initPromiseClass(classPo class, classPo request) {
  pthread_once(&promiseOnce, initPromiseEtc);
}

static void promiseInit(objectPo o, va_list *args) {
  promisePo f = O_PROMISE(o);

  promiseProc then = va_arg(*args, promiseProc);
  promiseProc fail = va_arg(*args, promiseProc);
  void *cd = va_arg(*args, void*);

  lockClass(f->object.class);

  f->pr.then = then;
  f->pr.fail = fail;
  f->pr.cd = cd;
  f->pr.status = idle;
  f->pr.next = f->pr.prev = Null;

  unlockClass(f->object.class);
}

void enqueuePromise(promisePo pr){
  lockClass(pr->object.class);

  PromiseClassRec* promiseCl = (PromiseClassRec*)(pr->object.class);

  assert(pr->pr.next == Null);

  if (promiseCl->promisePart.activeQ == NULL)
    promiseCl->promisePart.activeQ = pr->pr.next = pr->pr.prev = pr;
  else {
    pr->pr.next = promiseCl->promisePart.activeQ;
    pr->pr.prev = promiseCl->promisePart.activeQ->pr.prev;
    promiseCl->promisePart.activeQ->pr.prev->pr.next = pr;
    promiseCl->promisePart.activeQ->pr.prev = pr;
    promiseCl->promisePart.activeQ = pr;
  }
  unlockClass(pr->object.class);
}

promiseStatus statusOf(promisePo pr){
  return pr->pr.status;
}
