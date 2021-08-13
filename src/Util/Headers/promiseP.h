//
// Created by Francis McCabe on 8/11/21.
//

#ifndef STAR_PROMISEP_H
#define STAR_PROMISEP_H

#include "promise.h"
#include "objectP.h"                    /* access object system */
#include "lockableP.h"

typedef struct {
  promisePo activeQ;
} PromiseClassPartRec;

typedef struct _promise_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  PromiseClassPartRec promisePart;      /* the promise part of the class information */
} PromiseClassRec;

extern PromiseClassRec PromiseClass;    /* the standard pointer to a promise class record */

typedef struct {
  promiseStatus status;                 // What is the status of this promise
  promiseProc then;                     // Function to call when promise is fulfilled
  promiseProc fail;                     // Function to call when promise fails
  void *cd;                              // client data to pass to callbacks

  promisePo next;                       // next promise in chain
  promisePo prev;                       // previous promise in chain
} PromisePart;

typedef struct promise_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;                   // Lock part of object
  PromisePart pr;                       /* Promise level of promise object */
} PromiseObject;


void enqueuePromise(promisePo pr);

#endif //STAR_PROMISEP_H
