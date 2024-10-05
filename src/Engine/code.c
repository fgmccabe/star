//
// Created by Francis McCabe on 1/15/18.
//

#include <heapP.h>
#include <memory.h>
#include "array.h"
#include <arith.h>
#include "codeP.h"
#include "labelsP.h"
#include "debugP.h"
#include <assert.h>
#include <stdlib.h>
#include "quick.h"
#include "tpl.h"
#include "globals.h"
#include "decodeP.h"
#include "pkgP.h"

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

  return ((termPo) o) + mtdSize(cl, o);
}

termPo codeFinalizer(specialClassPo class, termPo o) {
  methodPo mtd = C_MTD(o);
  if (mtd->block != Null) {
    freeBlock(mtd->block);
    mtd->block = Null;
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

  normalPo pool = codeLits(mtd);
  if (pool != Null) {
    labelPo lbl = C_LBL(nthArg(pool, 0));
    return showLbl(out, lbl, 0, precision, alt);
  } else {
    outMsg(out, "<unknown mtd: ");
    dissassBlock(out, Null, codeLits(mtd), mtd->block, precision, depth, alt, "");
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

integer stackDelta(methodPo mtd) {
  assert(mtd != Null);
  return mtd->stackDelta;
}

logical pcInBlock(blockPo block, insPo pc) {
  if (pc >= block->ins && pc < &block->ins[block->insCount])
    return True;
  else {
    for (integer ix = 0; ix < block->insCount; ix++) {
#define sznOp
#define sztOs
#define szart
#define szi32
#define szarg
#define szlcl
#define szlcs
#define szsym
#define szEs
#define szlit
#define sztPe
#define szglb
#define szbLk if(pcInBlock(block->ins[ix].snd.block,pc)) return True;
#define szlVl

#define instruction(Op, A1, A2, Dl, Tp, Cmt)    \
      case Op:                              \
        sz##A1(Op)                          \
        sz##A2(Op)                          \
        break;

#include "instructions.h"

#undef instruction
#undef szi32
#undef szart
#undef szarg
#undef szlcl
#undef szlcs
#undef szbLk
#undef szlVl
#undef szsym
#undef szEs
#undef szlit
#undef sztPe
#undef szglb
#undef sznOp
#undef sztOs
    }
    return False;
  }
}

logical validPC(methodPo mtd, insPo pc) {
  return pcInBlock(entryBlock(mtd), pc);
}

static retCode findPcInBlock(blockPo block, insPo pc, char *prefix, char *buffer, integer buffLen) {
  if (pc >= block->ins && pc < &block->ins[block->insCount]) {
    strMsg(buffer, buffLen, "%s.%d", prefix, pc - &block->ins[0]);
    return Ok;
  } else {
    for (integer ix = 0; ix < block->insCount; ix++) {
#define sznOp
#define sztOs
#define szart
#define szi32
#define szarg
#define szlcl
#define szlcs
#define szsym
#define szEs
#define szlit
#define sztPe
#define szglb
#define szbLk { \
   char blockPrefix[MAXLINE]; strMsg(blockPrefix,NumberOf(blockPrefix),"%s.%d",prefix,pc-&block->ins[0]); \
   if(findPcInBlock(block->ins[ix].snd.block,pc,blockPrefix,NumberOf(blockPrefix))==Ok)                   \
     return Ok;                                                                                           \
}
#define szlVl

#define instruction(Op, A1, A2, Dl, Tp, Cmt)    \
      case Op:                              \
        sz##A1(Op)                          \
        sz##A2(Op)                          \
        break;

#include "instructions.h"

#undef instruction
#undef szi32
#undef szart
#undef szarg
#undef szlcl
#undef szlcs
#undef szbLk
#undef szlVl
#undef szsym
#undef szEs
#undef szlit
#undef sztPe
#undef szglb
#undef sznOp
#undef sztOs
    }
    return Fail;
  }
}

retCode findPcLocation(methodPo mtd, insPo pc, char *buffer, integer buffLen) {
  return findPcInBlock(entryBlock(mtd), pc, "", buffer, buffLen);
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

blockPo allocateBlock(integer insCount, blockPo parent, integer offset) {
  blockPo block = (blockPo) malloc(sizeof(InstructionBlock) + insCount * sizeof(Instruction));

  block->insCount = insCount;
  return block;
}

void freeBlock(blockPo block) {
  for (integer pc = 0; pc < block->insCount; pc++) {
    insPo ins = &block->ins[pc];
    if (ins->snd.block != Null)
      freeBlock(ins->snd.block);
  }
  free((void *) block);
}

methodPo
defineMtd(heapPo H, blockPo block, integer funSigIx, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool) {
  int root = gcAddRoot(H, (ptrPo) &lbl);
  gcAddRoot(H, (ptrPo) &pool);

  methodPo mtd = (methodPo) allocateObject(H, methodClass, MtdCellCount);

  mtd->entryCount = 0;
  mtd->block = block;
  mtd->jit = Null;
  mtd->sigIx = funSigIx;
  mtd->arity = labelArity(lbl);
  mtd->lclcnt = lclCount;
  mtd->pool = pool;
  mtd->stackDelta = stackDelta;

  lbl->mtd = mtd;

  gcReleaseRoot(H, root);

  return mtd;
}

methodPo declareMethod(const char *name, integer arity, blockPo block, termPo sigTerm, integer lclCount) {
  labelPo lbl = declareLbl(name, arity, 0);
  normalPo pool = allocateTpl(globalHeap, 2);
  setArg(pool, 0, (termPo) lbl);
  setArg(pool, 1, sigTerm);

  return defineMtd(globalHeap, block, 1, lclCount, 0, 0, pool);
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
