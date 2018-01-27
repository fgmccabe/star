//
// Created by Francis McCabe on 1/15/18.
//

#include "codeP.h"

static hashPo labels;
static poolPo labelPool;
static poolPo mtdPool;
static poolPo pkgPool;
static hashPo packages;

static integer labelHash(labelPo lbl);
static comparison labelCmp(labelPo lb1, labelPo lb2);
static retCode labelDel(labelPo lbl, labelPo l);
static retCode delPkg(packagePo pkg, pkgPo p);

void initCode() {
  labels = NewHash(1024, (hashFun) labelHash, (compFun) labelCmp, (destFun) labelDel);
  labelPool = newPool(sizeof(Label), 1024);
  mtdPool = newPool(sizeof(MethodRec), 1024);
  pkgPool = newPool(sizeof(PkgRec), 16);
  packages = NewHash(16, (hashFun) pkgHash, (compFun) compPkg, (destFun) delPkg);
}

extern methodPo C_MTD(termPo t) {
  assert(hasClass(t, methodClass));
  return (methodPo) t;
}

labelPo declareLbl(char *name, integer arity) {
  Label tst = {.name=name, .arity=arity};
  labelPo lbl = hashGet(labels, &tst);

  if (lbl == Null) {
    lbl = (labelPo) allocPool(labelPool);
    lbl->arity = arity;
    lbl->name = uniDuplicate(name);
    lbl->mtd = Null;
    lbl->clss = labelClass;
    hashPut(labels, lbl, lbl);
  }
  return lbl;
}

labelPo
defineMtd(insPo ins, integer insCount, char *name, integer arity, normalPo pool, normalPo frames, normalPo locals) {
  methodPo mtd = (methodPo) allocPool(mtdPool);
  mtd->clss = methodClass;
  mtd->code = ins;
  mtd->codeSize = insCount;
  mtd->jit = Null;
  mtd->arity = arity;
  mtd->pool = pool;
  mtd->frames = frames;
  mtd->locals = locals;

  labelPo lbl = declareLbl(name, arity);

  if (lbl->mtd != Null)
    freePool(mtdPool, lbl->mtd);

  lbl->mtd = mtd;

  return lbl;
}

integer labelHash(labelPo lbl) {
  return uniHash(lbl->name) * 37 + lbl->arity;
}

comparison labelCmp(labelPo lb1, labelPo lb2) {
  comparison comp = uniCmp(lb1->name, lb2->name);

  if (comp == same) {
    if (lb1->arity < lb2->arity)
      comp = smaller;
    else if (lb1->arity > lb2->arity)
      comp = bigger;
  }
  return comp;
}

retCode labelDel(labelPo lbl, labelPo l) {
  uniDestroy(lbl->name);
  freePool(labelPool, lbl);
  return Ok;
}

static void markMtd(heapPo h, methodPo mtd) {

}

static retCode markLabel(void *n, void *r, void *c) {
  labelPo lbl = (labelPo) r;
  heapPo h = (heapPo) c;

  if (lbl->mtd != Null)
    markMtd(h, lbl->mtd);
  return Ok;
}

void markLabels(heapPo heap) {
  ProcessTable(markLabel, labels, heap);
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

