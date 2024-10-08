//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include <memory.h>
#include <arith.h>
#include "codeP.h"
#include "labelsP.h"
#include "debugP.h"
#include <assert.h>
#include <stdlib.h>
#include "quick.h"
#include "tpl.h"
#include "globals.h"

static poolPo pkgPool;
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
  MethodClass.clss = specialClass;

  pkgPool = newPool(sizeof(PackageRec), 16);
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
  methodPo si = C_MTD(src);
  methodPo di = (methodPo) dst;
  *di = *si;

  return ((termPo) di) + mtdSize(cl, src);
}

termPo mtdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  methodPo mtd = C_MTD(o);

  helper((ptrPo) &mtd->pool, c);
  helper((ptrPo) &mtd->locals, c);
  helper((ptrPo) &mtd->lines, c);

  return ((termPo) o) + mtdSize(cl, o);
}

termPo codeFinalizer(specialClassPo class, termPo o) {
  methodPo mtd = C_MTD(o);
  free((void *) mtd->instructions);
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

  normalPo pool = codeLits(mtd);
  if (pool != Null) {
    labelPo lbl = C_LBL(nthArg(pool, 0));
    return showLbl(out, lbl, 0, precision, alt);
  } else {
    outMsg(out, "<unknown mtd: ");
    disass(out, Null, mtd, mtd->instructions);
    return outMsg(out, ">");
  }
}

labelPo mtdLabel(methodPo mtd) {
  normalPo pool = codeLits(mtd);
  if (pool != Null) {
    return C_LBL(nthArg(pool, 0));
  }
  return Null;
}

integer insOffset(methodPo m, insPo pc) {
  return (integer) (pc - m->instructions);
}

insPo pcAddr(methodPo mtd, integer pcOffset) {
  return &mtd->instructions[pcOffset];
}

integer stackDelta(methodPo mtd) {
  assert(mtd != Null);
  return mtd->stackDelta;
}

logical validPC(methodPo mtd, insPo pc) {
  return (logical) (pc >= mtd->instructions && pc < &mtd->instructions[mtd->codeSize]);
}

integer mtdCodeSize(methodPo mtd) {
  return mtd->codeSize;
}

termPo findPcLocation(methodPo mtd, integer pc) {
  normalPo lines = mtd->lines;
  if (lines != Null) {
    integer start = 0;
    integer limit = termArity(lines) - 1;

    integer lowerPc = -1;
    integer upperPc = mtdCodeSize(mtd);

    termPo lowerLoc = Null;
    termPo upperLoc = Null;

    while (limit >= start) {
      integer mid = start + (limit - start) / 2;
      normalPo midEntry = C_NORMAL(nthArg(lines, mid));
      integer testPc = integerVal(nthArg(midEntry, 1));
      termPo testLoc = nthArg(midEntry, 0);

      if (testPc == pc)
        return testLoc;
      else if (testPc < pc) {
        start = mid + 1;
        if (testPc > lowerPc) {
          lowerPc = testPc;
          lowerLoc = testLoc;
        }
      } else {
        limit = mid - 1;

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
  } else
    return Null;
}

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  return mtdDisp(f, (termPo) data, precision, depth, alt);
}

normalPo codeLits(methodPo mtd) {
  assert(mtd != Null);
  return mtd->pool;
}

logical normalCode(methodPo mtd) {
  return (logical) (mtd->pool != Null);
}

integer codeLitCount(methodPo mtd) {
  assert(mtd != Null && mtd->pool != Null);
  normalPo lits = mtd->pool;
  return termArity(lits);
}

termPo getMtdLit(methodPo mtd, integer litNo) {
  return nthArg(codeLits(mtd), litNo);
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

integer lclCount(methodPo mtd) {
  return mtd->lclcnt;
}

integer codeArity(methodPo mtd) {
  return mtd->arity;
}

methodPo
defineMtd(heapPo H, insPo ins, integer insCount, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool,
          normalPo locals, normalPo lines) {
  int root = gcAddRoot(H, (ptrPo) &lbl);
  gcAddRoot(H, (ptrPo) &pool);
  gcAddRoot(H, (ptrPo) &locals);
  gcAddRoot(H, (ptrPo) &lines);

  methodPo mtd = (methodPo) allocateObject(H, methodClass, MtdCellCount);

  size_t codeSize = insCount * sizeof(insWord);
  mtd->instructions = (insPo) malloc(codeSize);
  memcpy((void *) mtd->instructions, ins, codeSize);

  mtd->codeSize = insCount;
  mtd->jit = Null;
  mtd->arity = labelArity(lbl);
  mtd->lclcnt = lclCount;
  mtd->pool = pool;
  mtd->locals = locals;
  mtd->lines = lines;
  mtd->stackDelta = stackDelta;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

methodPo declareMethod(const char *name, integer arity, insPo ins, integer insCount) {
  labelPo lbl = declareLbl(name, arity, 0);
  normalPo pool = allocateTpl(globalHeap, 1);
  setArg(pool, 0, (termPo) lbl);

  return defineMtd(globalHeap, ins, insCount, 0, 0, lbl, pool, C_NORMAL(unitEnum), C_NORMAL(unitEnum));
}


static retCode showMtdCount(labelPo lbl, void *cl) {
  ioPo out = (ioPo) cl;
  methodPo mtd = labelCode(lbl);
  if (mtd != Null && callCount(mtd) > 0) {
    return outMsg(out, "%L %ld\n", lbl, callCount(mtd));
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

retCode setJitCode(methodPo mtd, jitCode code) {
  assert(!hasJit(mtd));
  mtd->jit = code;
  return Ok;
}
