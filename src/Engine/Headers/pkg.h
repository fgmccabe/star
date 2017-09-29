//
// Created by Francis McCabe on 2/23/17.
//

#ifndef CAFE_LOAD_H
#define CAFE_LOAD_H

#include "engine.h"

typedef struct _package_record_ {
  char packageName[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
} PackageRec, *packagePo;

void initPackages();

packagePo loadedPackage(char *package);

char *pkgName(packagePo pkg);
char *pkgVers(packagePo pkg);
char *loadedVersion(char *package);

#endif //CAFE_LOAD_H
