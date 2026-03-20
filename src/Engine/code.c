//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include "codeP.h"
#include <assert.h>
#include <stdlib.h>
#include "quick.h"
#include "disass.h"
#include "pkgP.h"

static poolPo pkgPool;
static poolPo mtdPool;
static hashPo packages;

static long mtdSize(builtinClassPo cl, termPo o);
static termPo mtdCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo mtdScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical mtdCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer mtdHash(builtinClassPo cl, termPo o);
static retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo codeFinalizer(builtinClassPo class, termPo o);

BuiltinTerm MethodClass = {
  .special = {0,0},
  .sizeFun = mtdSize,
  .copyFun = mtdCopy,
  .scanFun = mtdScan,
  .finalizer = codeFinalizer,
  .compFun = mtdCmp,
  .hashFun = mtdHash,
  .dispFun = mtdDisp
};

builtinClassPo methodClass = &MethodClass;
int32 methodIndex;

static retCode delPkg(packagePo pkg, packagePo p);
static integer pkHash(packagePo pkg);
static comparison compPk(packagePo p1, packagePo p2);

void initCode() {
  MethodClass.special.lblIndex = specialIndex;
  methodIndex = standardIndex(methodClass);

  pkgPool = newPool(sizeof(PackageRec), 16);
  mtdPool = newPool(sizeof(MethodRec), 4096);
  packages = newHash(16, (hashFun) pkHash, (compFun) compPk, (destFun) delPkg);
}

extern methodPo C_MTD(termPo t) {
  assert(hasIndex(t, methodIndex));
  return (methodPo) t;
}

long mtdSize(builtinClassPo cl, termPo o) {
  return MtdCellCount;
}

termPo mtdCopy(builtinClassPo cl, termPo dst, termPo src) {
  syserr("Should not be scanning code objects");
  methodPo si = C_MTD(src);
  methodPo di = (methodPo) dst;
  *di = *si;

  return ((termPo) di) + mtdSize(cl, src);
}

termPo mtdScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o) {
  syserr("Should not be scanning code objects");
  methodPo mtd = C_MTD(o);

  helper((ptrPo) &mtd->lbl, c);

  return ((termPo) o) + mtdSize(cl, o);
}

void markMethod(methodPo mtd, gcSupportPo G) {
}

termPo codeFinalizer(builtinClassPo class, termPo o) {
  methodPo mtd = C_MTD(o);
  if (mtd->instructions != Null) {
    free(mtd->instructions);
    mtd->instructions = Null;
  }

  return ((termPo) o) + mtdSize(class, o);
}

logical mtdCmp(builtinClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer mtdHash(builtinClassPo cl, termPo o) {
  return hash61((integer) o);
}

retCode mtdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  methodPo mtd = C_MTD(t);
  return outMsg(out, "%%%s/%d", mtd->lbl->lbl.name, mtd->lbl->lbl.arity);
}

labelPo mtdLabel(methodPo mtd) {
  return mtd->lbl;
}

logical mtdHasName(methodPo mtd,char *name) {
  return uniIsLit(lblName(mtdLabel(mtd)),name);
}

int32 stackDelta(methodPo mtd) {
  assert(mtd != Null);
  return mtd->stackDelta + mtd->lclcnt;
}

logical validPC(methodPo mtd, ssaInsPo pc) {
  return (logical) (pc >= mtd->instructions && pc < &mtd->instructions[mtd->insCount]);
}

int32 codeOffset(methodPo mtd, ssaInsPo pc) {
  ssaInsPo instructions = mtd->instructions;
  if (pc >= instructions && pc < instructions + mtd->insCount) {
    return (int32) (pc - instructions);
  }
#ifndef NOJIT
  void *address = pc;
  void *jitCode = (void *) mtd->jit.code;
  if (address >= jitCode && address < jitCode + mtd->jit.codeSize)
    return (int32) (address - jitCode);
#endif
  return -1;
}

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  return mtdDisp(f, (termPo) data, precision, depth, alt);
}

void showMethodCode(ioPo out, char *msg, methodPo mtd) {
  ssaInsPo pc = entryPoint(mtd);
  ssaInsPo last = entryPoint(mtd) + codeSize(mtd);

  outMsg(out, msg, mtdLabel(mtd));

  outMsg(out, "%d locals\n", lclCount(mtd));

  while (pc < last) {
    pc = disass(out, NULL, mtd, pc);
    outMsg(out, "\n");
  }
  flushOut();
}

int32 lclCount(methodPo mtd) {
  return mtd->lclcnt;
}

#ifndef NOJIT
logical hasJitCode(methodPo mtd) {
  return (logical) (mtd->jit.code != Null);
}
#endif

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

int32 mtdArity(methodPo mtd) {
  return lblArity(mtd->lbl);
}

int32 codeSize(methodPo mtd) {
  return mtd->insCount;
}

methodPo defineMtd(heapPo H, int32 insCount, ssaInsPo instructions, int32 lclCount, int32 stackLimit, labelPo lbl) {
  int root = gcAddRoot(H, (ptrPo) &lbl);

  methodPo mtd = (methodPo) allocPool(mtdPool);

  mtd->clss.lblIndex = methodIndex;
  mtd->insCount = insCount;
  mtd->instructions = instructions;
#ifndef NOJIT
  mtd->jit.code = Null;
  mtd->jit.codeSize = 0;
#endif
  mtd->lbl = lbl;
  mtd->lclcnt = lclCount;
  mtd->stackDelta = stackLimit;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

labelPo specialMethod(const char *name, int32 arity, int32 insCx, ssaInsPo instructions, int32 lcls, int32 stkLimit) {
  labelPo lbl = declareLbl(name, arity, 0);

  methodPo mtd = defineMtd(globalHeap, insCx, instructions, lcls, stkLimit, lbl);

#ifndef NOJIT
  char errMsg[MAXLINE];
  retCode ret = jitSpecial(mtd, errMsg, NumberOf(errMsg), depth);
  if (ret != Ok) {
    char msg[MAX_SYMB_LEN];
    strMsg(msg,NumberOf(msg), "could not generate jit code for special method %L,\nbecause %s", lbl, errMsg);
    syserr(msg);
  }
#endif

  return lbl;
}

#ifndef NOJIT
retCode setJitCode(methodPo mtd, jittedCode code, uint32 codeSize) {
  assert(!hasJit(mtd));
  mtd->jit.code = code;
  mtd->jit.codeSize = codeSize;
  return Ok;
}
#endif
