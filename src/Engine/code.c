//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include <memory.h>
#include "codeP.h"
#include <assert.h>
#include <stdlib.h>
#include "quick.h"
#include "pkgP.h"
#include "arith.h"

static poolPo pkgPool;
static poolPo mtdPool;
static hashPo packages;

static long mtdSize(specialClassPo cl, termPo o);
static termPo mtdCopy(specialClassPo cl, termPo dst, termPo src);
static termPo mtdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical mtdCmp(specialClassPo cl, termPo o1, termPo o2);
static integer mtdHash(specialClassPo cl, termPo o);
static retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo codeFinalizer(specialClassPo class, termPo o);

SpecialClass MethodClass = {
  .clss = Null,
  .sizeFun = mtdSize,
  .copyFun = mtdCopy,
  .scanFun = mtdScan,
  .finalizer = codeFinalizer,
  .compFun = mtdCmp,
  .hashFun = mtdHash,
  .dispFun = mtdDisp
};

clssPo methodClass = (clssPo) &MethodClass;

static retCode delPkg(packagePo pkg, packagePo p);
static integer pkHash(packagePo pkg);
static comparison compPk(packagePo p1, packagePo p2);

void initCode() {
  MethodClass.clss.clss = specialClass;

  pkgPool = newPool(sizeof(PackageRec), 16);
  mtdPool = newPool(sizeof(MethodRec), 4096);
  packages = newHash(16, (hashFun) pkHash, (compFun) compPk, (destFun) delPkg);
}

extern methodPo C_MTD(termPo t) {
  assert(hasClass(t, methodClass));
  return (methodPo) t;
}

long mtdSize(specialClassPo cl, termPo o) {
  return MtdCellCount;
}

termPo mtdCopy(specialClassPo cl, termPo dst, termPo src) {
  syserr("Should not be scanning code objects");
  methodPo si = C_MTD(src);
  methodPo di = (methodPo) dst;
  *di = *si;

  return ((termPo) di) + mtdSize(cl, src);
}

termPo mtdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  syserr("Should not be scanning code objects");
  methodPo mtd = C_MTD(o);

  helper((ptrPo) &mtd->lbl, c);
  helper((ptrPo) &mtd->locs, c);

  return ((termPo) o) + mtdSize(cl, o);
}

void markMethod(methodPo mtd, gcSupportPo G) {
  if (mtd->locs != Null)
    mtd->locs = markPtr(G, &mtd->locs);
}

termPo codeFinalizer(specialClassPo class, termPo o) {
  methodPo mtd = C_MTD(o);
  if (mtd->instructions != Null) {
    free(mtd->instructions);
    mtd->instructions = Null;
  }

  return ((termPo) o) + mtdSize(class, o);
}

logical mtdCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer mtdHash(specialClassPo cl, termPo o) {
  return hash61((integer) o);
}

retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  methodPo mtd = C_MTD(t);
  return outMsg(out,"%%%s/%d",mtd->lbl->lbl.name,mtd->lbl->lbl.arity);
}

labelPo mtdLabel(methodPo mtd) {
  return mtd->lbl;
}

integer stackDelta(methodPo mtd) {
  assert(mtd != Null);
  return mtd->stackDelta + mtd->lclcnt;
}

logical validPC(methodPo mtd, insPo pc) {
  return (logical) (pc >= mtd->instructions && pc < &mtd->instructions[mtd->insCount]);
}

int32 codeOffset(methodPo mtd, insPo pc) {
  assert(validPC(mtd, pc));
  return (int32) (pc - mtd->instructions);
}

termPo findPcLocation(methodPo mtd, int32 pc) {
  if (mtd->locs != Null && isNormalPo(mtd->locs)) {
    normalPo locs = C_NORMAL(mtd->locs);

    int32 start = 0;
    int32 limit = termArity(locs) - 1;

    int32 lowerPc = -1;
    int32 upperPc = codeSize(mtd);

    termPo lowerLoc = Null;
    termPo upperLoc = Null;

    while (limit >= start) {
      int32 mid = start + (limit - start) / 2;

      normalPo midEntry = C_NORMAL(nthArg(locs, mid));

      int32 testPc = (int32) integerVal(nthArg(midEntry, 0));

      if (testPc == pc)
        return nthArg(midEntry, 1);
      else if (testPc < pc) {
        start = mid + 1;
        if (testPc > lowerPc) {
          lowerPc = testPc;
          lowerLoc = nthArg(midEntry, 1);
        }
      } else {
        limit = mid - 1;

        if (testPc < upperPc) {
          upperPc = testPc;
          upperLoc = nthArg(midEntry, 1);
        }
      }
    }
    if (lowerLoc != Null)
      return lowerLoc;
    else if (upperLoc != Null)
      return upperLoc;
    else
      return Null;
  } else
    return Null;
}

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  return mtdDisp(f, (termPo) data, precision, depth, alt);
}

integer callCount(methodPo mtd) {
  return mtd->entryCount;
}

packagePo loadedPackage(const char *package) {
  return (packagePo) hashGet(packages, (void *) package);
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

int32 codeArity(methodPo mtd) {
  return lblArity(mtd->lbl);
}

int32 codeSize(methodPo mtd) {
  return mtd->insCount;
}

methodPo
defineMtd(heapPo H, int32 insCount, insPo instructions, int32 lclCount, int32 stackHeight, labelPo lbl, termPo locs) {
  int root = gcAddRoot(H, (ptrPo) &lbl);

  methodPo mtd = (methodPo) allocPool(mtdPool);

  mtd->clss.clss = methodClass;
  mtd->entryCount = 0;
  mtd->insCount = insCount;
  mtd->instructions = instructions;
  mtd->jit = Null;
  mtd->lbl = lbl;
  mtd->lclcnt = lclCount;
  mtd->locs = locs;
  mtd->stackDelta = stackHeight;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

labelPo specialMethod(const char *name, int32 arity, int32 insCx, insPo instructions, termPo sigTerm, int32 lcls) {
  labelPo lbl = declareLbl(name, arity, 0);

  defineMtd(globalHeap, insCx, instructions, 0, 0, lbl, Null);
  return lbl;
}

static retCode showMtdCount(labelPo lbl, void *cl) {
  ioPo out = (ioPo) cl;
  methodPo mtd = labelCode(lbl);
  if (mtd != Null && callCount(mtd) > 0) {
    return outMsg(out, "%A %ld\n", lbl, callCount(mtd));
  } else
    return Ok;
}

static comparison cmpCount(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  methodPo mi = labelCode(&labelTable[indices[i]]);
  methodPo mj = labelCode(&labelTable[indices[j]]);

  integer iCount = (mi == Null ? 0 : callCount(mi));
  integer jCount = (mj == Null ? 0 : callCount(mj));

  if (iCount < jCount)
    return smaller;
  else if (iCount == jCount)
    return same;
  else
    return bigger;
}

static retCode swapIndex(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  integer w = indices[i];
  indices[i] = indices[j];
  indices[j] = w;
  return Ok;
}

void showMtdCounts(ioPo out) {
  outMsg(out, "sorted method counts\n");

  integer indices[lblTableTop];
  for (int ix = 0; ix < lblTableTop; ix++)
    indices[ix] = ix;

  quick(0, lblTableTop - 1, cmpCount, swapIndex, (void *) indices);
  for (integer ix = 0; ix < lblTableTop; ix++) {
    showMtdCount(&labelTable[indices[ix]], out);
  }
}

retCode setJitCode(methodPo mtd, jittedCode code) {
  assert(!hasJit(mtd));
  mtd->jit = code;
  return Ok;
}
