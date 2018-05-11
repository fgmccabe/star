//
// Created by Francis McCabe on 2/23/17.
//

#ifndef CAFE_PKG_H
#define CAFE_PKG_H

#include "ooio.h"

typedef struct _package_record_ *packagePo;

char *pkgName(packagePo pkg);
char *pkgVers(packagePo pkg);

integer pkgHash(packagePo pkg);
comparison compPkg(packagePo p1,packagePo p2);

logical compatiblVersion(char *rqVer, char *ver);

#endif
