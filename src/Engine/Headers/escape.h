//
// Created by Francis McCabe on 2/11/24.
//

#ifndef STAR_ESCAPE_H
#define STAR_ESCAPE_H

#include "engine.h"

typedef enum {
  Normal,
  Abnormal
} ReturnStatus;

// The order of these is important.
typedef struct {
  termPo value;
  ReturnStatus status;
} ValueReturn;

#define normalReturn(R) ((ValueReturn){.status=Normal, .value=(R)})
#define abnormalReturn(R) ((ValueReturn){.status=Abnormal, .value=(R)})


typedef ReturnStatus (*escFun)(enginePo p);

typedef ValueReturn (*escValue)(enginePo p);

typedef struct escape_record_ *escapePo;

escapePo getEscape(uint32 escNo);
char *escapeName(escapePo esc);
int32 escapeArity(escapePo esc);
escFun escapeFun(escapePo esc);

#endif //STAR_ESCAPE_H
