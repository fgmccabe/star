//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include <memory.h>
#include <arith.h>
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
  helper((ptrPo) &mtd->lines, c);

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
  if (pool != Null) {
    labelPo lbl = C_LBL(nthArg(pool, 0));
    return showLbl(out, precision, alt, lbl);
  } else
    return outMsg(out, "<unknown mtd>");
}

methodPo
defineMtd(heapPo H, insPo ins, integer insCount, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool,
          normalPo locals, normalPo lines) {
  int root = gcAddRoot(H, (ptrPo) &lbl);
  gcAddRoot(H, (ptrPo) &pool);
  gcAddRoot(H, (ptrPo) &locals);
  gcAddRoot(H, (ptrPo) &lines);

  methodPo mtd = (methodPo) allocateObject(H, methodClass, MtdCellCount(insCount));

  for (integer ix = 0; ix < insCount; ix++)
    mtd->code[ix] = ins[ix];

  mtd->codeSize = insCount;
  mtd->jit = Null;
  mtd->arity = lbl->arity;
  mtd->lclcnt = lclCount;
  mtd->pool = pool;
  mtd->locals = locals;
  mtd->lines = lines;
  mtd->stackDelta = stackDelta;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

void markMtd(gcSupportPo G, methodPo mtd) {

}

integer insOffset(methodPo m, insPo pc) {
  return (integer) (pc - &m->code[0]);
}

insPo pcAddr(methodPo mtd, integer off) {
  return &mtd->code[off];
}

logical validPC(methodPo mtd, insPo pc) {
  return (logical) (pc >= mtd->code && pc < &mtd->code[mtd->codeSize]);
}

integer mtdCodeSize(methodPo mtd){
  return mtd->codeSize;
}

termPo findPcLocation(methodPo mtd, integer pc) {
  normalPo lines = mtd->lines;
  integer start = 0;
  integer limit = termArity(lines)-1;

  integer lowerPc = -1;
  integer upperPc = mtdCodeSize(mtd);

  termPo lowerLoc = Null;
  termPo upperLoc = Null;

  while (limit >= start) {
    integer mid = start + (limit - start) / 2;
    normalPo midEntry = C_TERM(nthArg(lines, mid));
    integer testPc = integerVal(nthArg(midEntry, 1));
    termPo testLoc = nthArg(midEntry, 0);

    if (testPc == pc)
      return testLoc;
    else if (testPc < pc) {
      start = mid+1;
      if (testPc > lowerPc) {
        lowerPc = testPc;
        lowerLoc = testLoc;
      }
    } else {
      limit = mid-1;

      if (testPc < upperPc) {
        upperPc = testPc;
        upperLoc = testLoc;
      }
    }
  }
  if (lowerLoc != Null)
    return lowerLoc;
  else
    return upperLoc;
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
  return termArity(lits);
}

termPo getMtdLit(methodPo mtd, integer litNo) {
  return nthArg(codeLits(mtd), litNo);
}

packagePo loadedPackage(const char *package) {
  return (packagePo) hashGet(packages, (void*)package);
}

logical isLoadedPackage(packagePo pkg) {
  packagePo lcl = loadedPackage(pkg->packageName);
  if (lcl != Null) {
    return compatiblePkg(pkg, lcl);
  } else
    return False;
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

integer codeArity(methodPo mtd) {
  return mtd->arity;
}
