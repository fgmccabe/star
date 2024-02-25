//
// Created by Francis McCabe on 9/21/20.
//

#ifndef STAR_LTYPE_H
#define STAR_LTYPE_H

#include "engine.h"

typedef enum{
  int64Tp = 'i',
  flt64Tp = 'f',
  boolTp = 'l',
  ptrTp = 'p',
  funTp = 'F',
  tplTp = '('
} LTipe;

retCode validTypeSig(const char *text, integer len);
retCode skipTypeSig(const char *text, integer len, integer *pos);
retCode typeSigArity(const char *sig, integer len, integer *arity);

#endif //STAR_LTYPE_H
