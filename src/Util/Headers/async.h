//
// Created by Francis McCabe on 8/11/21.
//

#ifndef STAR_ASYNC_H
#define STAR_ASYNC_H

#include "object.h"
#include "integer.h"
#include "retcode.h"

typedef struct _async_object_ *asyncPo;
extern classPo asyncClass;

typedef enum {
  idle,
  inProgress,
  completed,
  failed
} asyncStatus;

typedef  retCode (*asyncProc)(asyncPo f);

asyncStatus statusOf(asyncPo);

retCode startRdChar(asyncPo f, asyncProc cb, codePoint *tgt);     /* read a character */

#endif //STAR_ASYNC_H
