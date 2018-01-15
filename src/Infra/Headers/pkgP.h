//
// Created by Francis McCabe on 2/23/17.
//

#ifndef CAFE_PKGP_H
#define CAFE_PKGP_H

#include "pkg.h"
#include "cafeOptions.h"

typedef struct _package_record_ {
  char packageName[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
} PackageRec;

#endif //CAFE_PKGP_H
