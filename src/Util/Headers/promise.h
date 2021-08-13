//
// Created by Francis McCabe on 8/11/21.
//

#ifndef STAR_PROMISE_H
#define STAR_PROMISE_H
#include "retcode.h"
#include "integer.h"
#include "object.h"
#include "lockable.h"

/*
 *  This is intended to be an abstract class. Specific forms of Promise (e.g., ioPromise) are sub-classed from
 *  the Promise class.
 */

typedef struct promise_object_ *promisePo;
extern classPo promiseClass;

typedef enum {
  idle,
  inProgress,
  completed,
  failed
} promiseStatus;

typedef retCode (*promiseProc)(promisePo f, void *cd);

promiseStatus statusOf(promisePo pr);

retCode attachHndlers(promisePo promise,promiseProc then,promisePo fail, void *cd);

#ifdef VERIFY_OBJECT
#define O_PROMISE(c) ((promisePo)(checkCast((c),promiseClass)))
#else
#define O_PROMISE(c) ((promisePo)(c))
#endif

#endif //STAR_PROMISE_H
