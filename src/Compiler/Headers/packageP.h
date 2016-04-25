#ifndef _PACKAGE_P_H_
#define _PACKAGE_P_H_

#include "package.h"

typedef struct _package_ {
  uniChar *url;
  uniChar *name;
  dictPo dict;
  packageFun entry;
} PackageRec;

void initPackages();

#endif
