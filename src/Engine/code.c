//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include <memory.h>
#include "codeP.h"
#include "labelsP.h"

static poolPo pkgPool;
static hashPo packages;

static long mtdSize(specialClassPo cl, termPo o);
static termPo mtdCopy(specialClassPo cl, termPo dst, termPo src);
static termPo mtdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison mtdCmp(specialClassPo cl, termPo o1, termPo o2);
static integer mtdHash(specialClassPo cl, termPo o);
static retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass MethodClass = {
  .clss = Null,
  .sizeFun = mtdSize,
  .copyFun = mtdCopy,
  .scanFun = mtdScan,
  .compFun = mtdCmp,
  .hashFun = mtdHash,
  .dispFun = mtdDisp
};

clssPo methodClass = (clssPo) &MethodClass;

static retCode delPkg(packagePo pkg, packagePo p);
static integer pkHash(packagePo pkg);
static comparison compPk(packagePo p1, packagePo p2);

void initCode() {
  MethodClass.clss = specialClass;

  pkgPool = newPool(sizeof(PackageRec), 16);
  packages = NewHash(16, (hashFun) pkHash, (compFun) compPk, (destFun) delPkg);
}

extern methodPo C_MTD(termPo t) {
  assert(hasClass(t, methodClass));
  return (methodPo) t;
}

long mtdSize(specialClassPo cl, termPo o) {
  methodPo mtd = C_MTD(o);

  return MtdCellCount(mtd->codeSize);
}

termPo mtdCopy(specialClassPo cl, termPo dst, termPo src) {
  methodPo si = C_MTD(src);
  methodPo di = (methodPo) dst;
  *di = *si;

  memcpy(&di->code, &si->code, si->codeSize * sizeof(insWord));

  return ((termPo) di) + mtdSize(cl, src);
}

termPo mtdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  methodPo mtd = C_MTD(o);

  helper((ptrPo) &mtd->pool, c);
  helper((ptrPo) &mtd->locals, c);

  return ((termPo) o) + mtdSize(cl, o);
}

comparison mtdCmp(specialClassPo cl, termPo o1, termPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

integer mtdHash(specialClassPo cl, termPo o) {
  return (integer) o;
}

retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  methodPo mtd = C_MTD(t);
  normalPo pool = codeLits(mtd);
  labelPo lbl = C_LBL(nthArg(pool, 0));

  return showLbl(out, precision, alt, lbl);
}

methodPo
defineMtd(heapPo H, insPo ins, integer insCount, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool,
          normalPo locals) {
  int root = gcAddRoot(H, (ptrPo) &lbl);
  gcAddRoot(H, (ptrPo) &pool);
  gcAddRoot(H, (ptrPo) &locals);

  methodPo mtd = (methodPo) allocateObject(H, methodClass, MtdCellCount(insCount));

  for (integer ix = 0; ix < insCount; ix++)
    mtd->code[ix] = ins[ix];

  mtd->codeSize = insCount;
  mtd->jit = Null;
  mtd->arity = lbl->arity;
  mtd->lclcnt = lclCount;
  mtd->pool = pool;
  mtd->locals = locals;
  mtd->stackDelta = stackDelta;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

void markMtd(gcSupportPo G, methodPo mtd) {

}

logical validPC(methodPo mtd, insPo pc) {
  return (logical) (pc >= mtd->code && pc < &mtd->code[mtd->codeSize]);
}

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  methodPo mtd = (methodPo) data;
  normalPo pool = codeLits(mtd);
  termPo lbl = nthArg(pool, 0);

  return outMsg(f, "%T", lbl);
}

normalPo codeLits(methodPo mtd) {
  assert(mtd != Null);
  return mtd->pool;
}

integer codeLitCount(methodPo mtd) {
  assert(mtd != Null && mtd->pool != Null);
  normalPo lits = mtd->pool;
  return termSize(lits);
}

termPo getMtdLit(methodPo mtd, integer litNo) {
  return nthArg(codeLits(mtd), litNo);
}

packagePo loadedPackage(char *package) {
  return (packagePo) hashGet(packages, package);
}

packagePo createPkg(char *name, char *version) {
  packagePo pkg = (packagePo) allocPool(pkgPool);
  uniCpy((char *) &pkg->packageName, NumberOf(pkg->packageName), name);
  uniCpy((char *) &pkg->version, NumberOf(pkg->version), version);
  hashPut(packages, pkg, pkg);
  return pkg;
}

retCode delPkg(packagePo pkg, packagePo p) {
  freePool(pkgPool, p);
  return Ok;
}

char *loadedVersion(char *package) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL)
    return (char *) &pkg->version;

  return NULL;
}

integer pkHash(packagePo pkg) {
  return uniHash(pkg->packageName);
}

comparison compPk(packagePo p1, packagePo p2) {
  return uniCmp(p1->packageName, p2->packageName);
}

packagePo markLoaded(char *package, char *version) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL) {
    if (!compatiblVersion((char *) &pkg->version, version))
      return Null;
    else
      return pkg;
  } else
    return createPkg(package, version);
}

integer lclCount(methodPo mtd) {
  return mtd->lclcnt;
}
