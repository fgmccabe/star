//
// Created by Francis McCabe on 2/11/24.
//

#ifndef STAR_ESCAPE_H
#define STAR_ESCAPE_H

#include "stack.h"

typedef enum {
  Normal,
  Abnormal
} ReturnStatus;

typedef ReturnStatus (*escFun)(processPo p);

typedef struct escape_record_ *escapePo;

escapePo getEscape(uint32 escNo);
char *escapeName(escapePo esc);
int32 escapeArity(escapePo esc);
escFun escapeFun(escapePo esc);

#endif //STAR_ESCAPE_H
