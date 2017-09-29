#ifndef _PACKAGE_P_H_
#define _PACKAGE_P_H_

#include "package.h"

typedef struct _package_ {
  char *url;
  char *name;
  dictPo dict;
  packageFun entry;
} PackageRec;

void initPackages();

#endif
