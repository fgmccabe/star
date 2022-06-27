//
// Created by Francis McCabe on 8/11/21.
//

#ifndef STAR_ASYNC_H
#define STAR_ASYNC_H

#include "object.h"
#include "integer.h"
#include "retcode.h"
#include "unistr.h"

typedef struct async_object__ *asyncPo;
extern classPo asyncClass;

typedef enum {
  idle,
  inProgress,
  completed,
  failed
} asyncStatus;

typedef  retCode (*asyncProc)(asyncPo f);

asyncStatus statusOf(asyncPo);

retCode startRdChars(asyncPo f, asyncProc cb, integer count, codePoint *tgt);     /* read a character */

#endif //STAR_ASYNC_H
