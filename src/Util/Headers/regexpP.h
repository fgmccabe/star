//
// Created by Francis McCabe on 1/8/20.
//

#ifndef STAR_REGEXPP_H
#define STAR_REGEXPP_H

#include "regexp.h"

typedef enum {
  empty,
  dot,
  disj,
  seq,
  star,
  literal
} regexpOp;

typedef struct _regexp_ {
  regexpOp op;
  union{
    codePoint chr;
    struct {
      regexpPo left;
      regexpPo right;
    } choice;
  } arg;
} Regexp;

#endif //STAR_REGEXPP_H
