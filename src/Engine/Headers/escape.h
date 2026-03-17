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
  ReturnStatus status;
  termPo value;
} ValueReturn;

#define normalReturn(R) ((ValueReturn){.status=Normal, .value=(R)})
#define abnormalReturn(R) ((ValueReturn){.status=Abnormal, .value=(R)})

typedef ReturnStatus (*escFun)(enginePo p);

typedef ValueReturn (*escValue)(enginePo p);
typedef ValueReturn (*escValue1)(enginePo p, termPo);
typedef ValueReturn (*escValue2)(enginePo p, termPo, termPo);
typedef ValueReturn (*escValue3)(enginePo p, termPo, termPo, termPo);
typedef ValueReturn (*escValue4)(enginePo p, termPo, termPo, termPo, termPo);
typedef ValueReturn (*escValue5)(enginePo p, termPo, termPo, termPo, termPo, termPo);
typedef ValueReturn (*escValue6)(enginePo p, termPo, termPo, termPo, termPo, termPo, termPo);
typedef ValueReturn (*escValue7)(enginePo p, termPo, termPo, termPo, termPo, termPo, termPo, termPo);
typedef ValueReturn (*escValue8)(enginePo p, termPo, termPo, termPo, termPo, termPo, termPo, termPo, termPo);

typedef struct escape_record_ *escapePo;

escapePo getEscape(uint32 escNo);
char *escapeName(escapePo esc);
int32 escapeArity(escapePo esc);
escValue escapeCode(escapePo esc);

#endif //STAR_ESCAPE_H
