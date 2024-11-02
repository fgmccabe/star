//
// Created by Francis McCabe on 9/21/20.
//

#ifndef STAR_LTYPE_H
#define STAR_LTYPE_H

#include "engine.h"

typedef enum {
  int64Tp = 'i',
  flt64Tp = 'f',
  boolTp = 'l',
  ptrTp = 'p',
  funTp = 'F',
  tplTp = '(',
  vdTp = 'v',
  parTp = '%'
} LTipe;

retCode validTypeSig(const char *text, integer len);
retCode skipTypeSig(const char *text, integer len, integer *pos);
retCode typeSigArity(const char *sig, integer len, int32 *arity);
retCode showLSig(ioPo out, const char *sig, integer len);

retCode showLS(ioPo f, void *data, long depth, long precision, logical alt);

retCode funArgSig(const char *text, integer len, integer *pos);
retCode funResSig(const char *text, integer len, integer *pos);
logical isTupleSig(const char *text, integer len);

#endif //STAR_LTYPE_H
