//
// Created by Francis McCabe on 2/3/17.
//

#ifndef LANDO_JSONEVENT_H
#define LANDO_JSONEVENT_H

#include "retcode.h"
#include "unicode.h"
#include "io.h"

typedef retCode (*collectionProc)(void *cl);
typedef retCode (*entryProc)(const char *name, void *cl);
typedef retCode (*stringProc)(const char *str, void *cl);
typedef retCode (*numProc)(double dx, void *cl);
typedef retCode (*logicalProc)(logical trueVal, void *cl);
typedef retCode (*nullProc)(void *cl);

typedef struct {
  collectionProc startJson;
  collectionProc endJson;
  collectionProc startCollection;
  collectionProc endCollection;
  collectionProc startArray;
  collectionProc endArray;
  entryProc startEntry;
  entryProc endEntry;
  stringProc stringEntry;
  numProc numEntry;
  logicalProc logicalEntry;
  nullProc nullEntry;
  stringProc errorEntry;
} JsonCallBacks, *jsonCallBackPo;

int yyparse(ioPo in, jsonCallBackPo cb, void *client);

#endif //LANDO_JSONEVENT_H
