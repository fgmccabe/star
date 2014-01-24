#ifndef _PACKAGE_H_
#define _PACKAGE_H_

#include "config.h"
#include <ooio.h>
#include "compile.h"

typedef struct _package_ *packagePo;

typedef void (*packageFun)(dictPo dict);

packagePo findPackage(uniChar *path);

extern retCode ppPackage(ioPo io,packagePo pkg,int d);

extern uniChar *pkgName(packagePo pkg);
extern dictPo pkgDict(packagePo pkg);
extern packageFun pkgEntry(packagePo pkg);

#endif
