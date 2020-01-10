//
// Created by Francis McCabe on 1/8/20.
//

#ifndef STAR_REGEXP_H
#define STAR_REGEXP_H

#include "unistr.h"
#include "io.h"

typedef struct _regexp_ *regexpPo;

regexpPo parseRegexp(char *ptn,integer ptnLength);
void closeRegexp(regexpPo reg);
retCode matchRegexp(regexpPo r,ioPo in);

#endif //STAR_REGEXP_H
