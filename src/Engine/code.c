//
// Created by Francis McCabe on 1/15/18.
//

#include "codeP.h"
#include "labels.h"

static poolPo mtdPool;
static poolPo pkgPool;
static hashPo packages;

static retCode delPkg(packagePo pkg, pkgPo p);

void initCode() {
  mtdPool = newPool(sizeof(MethodRec), 1024);
  pkgPool = newPool(sizeof(PkgRec), 16);
  packages = NewHash(16, (hashFun) pkgHash, (compFun) compPkg, (destFun) delPkg);
}

extern methodPo C_MTD(termPo t) {
  assert(hasClass(t, methodClass));
  return (methodPo) t;
}

labelPo
defineMtd(insPo ins, integer insCount, char *name, integer arity, normalPo pool, normalPo locals) {
  methodPo mtd = (methodPo) allocPool(mtdPool);
  mtd->clss = methodClass;
  mtd->code = ins;
  mtd->codeSize = insCount;
  mtd->jit = Null;
  mtd->arity = arity;
  mtd->pool = pool;
  mtd->locals = locals;

  labelPo lbl = declareLbl(name, arity);

  if (lbl->mtd != Null)
    freePool(mtdPool, lbl->mtd);

  lbl->mtd = mtd;

  return lbl;
}

void markMtd(heapPo h, methodPo mtd) {

}

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  methodPo mtd = (methodPo) data;
  normalPo pool = codeConstants(mtd);
  termPo lbl = nthArg(pool, 0);

  return outMsg(f, "%W", lbl);
}

pkgPo loadedPackage(char *package) {
  return (pkgPo) hashGet(packages, package);
}

pkgPo createPkg(char *name, char *version) {
  pkgPo pkg = (pkgPo) allocPool(pkgPool);
  uniCpy((char *) &pkg->pkg.packageName, NumberOf(pkg->pkg.packageName), name);
  uniCpy((char *) &pkg->pkg.version, NumberOf(pkg->pkg.version), version);
  hashPut(packages, &pkg->pkg, pkg);
  return pkg;
}


retCode delPkg(packagePo pkg, pkgPo p){
  freePool(pkgPool,p);
  return Ok;
}

char *loadedVersion(char *package) {
  pkgPo pkg = loadedPackage(package);

  if (pkg != NULL)
    return (char *) &pkg->pkg.version;

  return NULL;
}

pkgPo markLoaded(char *package, char *version) {
  pkgPo pkg = loadedPackage(package);

  if (pkg != NULL) {
    if (!compatiblVersion((char *) &pkg->pkg.version, version))
      return Null;
    else
      return pkg;
  } else
    return createPkg(package, version);
}

