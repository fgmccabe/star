//
// Created by Francis McCabe on 2/23/17.
//

#ifndef STAR_PKGP_H
#define STAR_PKGP_H

#include "pkg.h"
#include "starOptions.h"

typedef struct _package_record_ {
  char packageName[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
} PackageRec;

static inline PackageRec makePkg(char *name,char *vers){
  PackageRec pkg;

  uniCpy(pkg.packageName,NumberOf(pkg.packageName),name);
  uniCpy(pkg.version,NumberOf(pkg.version),vers);

  return pkg;
}

extern packagePo loadedPackage(const char *package);
extern char *loadedVersion(char *package);

extern packagePo markLoaded(char *package, char *version);
extern logical isLoadedPackage(packagePo pkg);

extern retCode dispPkgNm(ioPo f, void *data, long depth, long precision, logical alt);

retCode loadPackage(packagePo p, char *errorMsg, long msgSize, void *cl);

typedef retCode (*pickupPkg)(packagePo pkg, char *errorMsg, long msgLen, void *cl);
retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl);

extern tracingLevel tracePkg;

#endif //STAR_PKGP_H
