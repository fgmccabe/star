/*
 * Assembler for the Cafe machine code
 */

#include "ooio.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <assemP.h>
#include "formioP.h"
#include "lists.h"

#include "assemP.h"
#include "encoding.h"
#include "esc.h"
#include "ops.h"

static poolPo pkgPool;      /* pool of packages */
static poolPo mtdPool;      /* pool of method blocks */
static poolPo insPool;      /* pool of instructions */
static poolPo constPool;    /* pool of constant values */
static poolPo varPool;      /* pool of local variable records */
static poolPo lblPool;      /* pool of labels */

static retCode displayLabel(ioPo f, void *p, long width, long prec, logical alt);

static char stringSig[] = {strSig, 0};
static char integerSig[] = {intSig, 0};
static char floatSig[] = {fltSig, 0};

void initAssem() {
  pkgPool = newPool(sizeof(AssemPackage), 3);
  mtdPool = newPool(sizeof(AssemMethod), 16);
  insPool = newPool(sizeof(AssemInstruction), 1024);
  constPool = newPool(sizeof(PoolConstantRecord), 256);
  varPool = newPool(sizeof(LocalVarRecord), 256);
  lblPool = newPool(sizeof(LabelRec), 128);

  installMsgProc('B', displayLabel);
  initEscapes();
}

static retCode delLabel(void *n, void *r);

pkgPo newPkg(char *name) {
  pkgPo pkg = (pkgPo) allocPool(pkgPool);
  pkg->name = uniDuplicate(name);
  pkg->methods = nilList;
  return pkg;
}

static logical isRightMethod(objectPo d, void *cl) {
  return (logical) (uniCmp(((mtdPo) d)->name, (char *) cl) == same);
}

mtdPo getPkgMethod(pkgPo pkg, char *name) {
  return (mtdPo) findInList(pkg->methods, isRightMethod, name);
}

static void defineSig(mtdPo mtd, char *sig);

mtdPo defineMethod(pkgPo pkg, logical public, char *name, int arity, char *sig, char *free) {
  mtdPo existing = getPkgMethod(pkg, name);

  if (existing == Null) {
    mtdPo mtd = (mtdPo) allocPool(mtdPool);
    mtd->name = uniDuplicate(name);
    mtd->labels = NewHash(16, (hashFun) uniHash, (compFun) uniCmp, delLabel);
    mtd->constants = nilList;
    mtd->first = mtd->last = Null;
    mtd->owner = pkg;
    mtd->sig = -1;
    mtd->locals = nilList;
    pkg->methods = tack((objectPo) mtd, pkg->methods);

    if (sig != Null) {
      defineSig(mtd, sig);    /* should be the first constant */
      mtd->freeSig = newStringConstant(mtd, free);
    }

    return mtd;
  } else if (existing->sig < 0 && sig != Null) {
    defineSig(existing, sig);
    existing->freeSig = newStringConstant(existing, free);
  }

  return existing;
}

static void defineSig(mtdPo mtd, char *sig) {
  assert(mtd->sig < 0);

  mtd->sig = newStringConstant(mtd, sig);
  assert(mtd->sig == 0);
}

static assemInsPo asm_i32(mtdPo mtd, OpCode op, int32 ix);

void defineFrame(mtdPo mtd, char *sig) {
  asm_i32(mtd, frame, newStringConstant(mtd, sig));
}

int32 frameCount(mtdPo mtd) {
  int32 cx = 0;
  assemInsPo ins = mtd->first;

  while (ins != Null) {
    if (ins->op == frame)
      cx++;
    ins = ins->next;
  }
  return cx;
}

void defineLocal(mtdPo mtd, char *name, char *sig, int32 off, lPo from, lPo to) {
  localVarPo var = (localVarPo) allocPool(varPool);
  var->name = newStringConstant(mtd, name);
  var->sig = newStringConstant(mtd, sig);
  var->off = off;
  var->from = from;
  var->to = to;
  mtd->locals = cons((objectPo) var, mtd->locals);
}

retCode delFunction(pkgPo pkg, mtdPo mtd) {
  pkg->methods = removeElements(pkg->methods, isRightMethod, mtd->name);

  DelHash(mtd->labels);

  assemInsPo ins = mtd->first;
  while (ins != Null) {
    assemInsPo next = ins->next;
    freePool(insPool, ins);
    ins = next;
  }

  listPo cons = mtd->constants;
  while (cons != nilList) {
    freePool(constPool, head(cons));
    cons = tail(cons);
  }
  releaseList(mtd->constants);

  freePool(mtdPool, mtd);
  return Ok;
}

static retCode procLbl(void *n, void *r, void *c) {
  lPo lbl = (lPo) r;
  if (lbl->pc == Null)
    outMsg(logFile, "label %B undefined\n", lbl);
  return Ok;
}

static void fixup(assemInsPo ins);

void endFunction(mtdPo mtd) {
  // Phase one: compute final program counter for each instruction
  int32 pc = 0;
  assemInsPo ins = mtd->first;

  while (ins != Null) {
    ins->pc = pc;

    switch (ins->op) {

#undef instruction

#define sztos
#define sznOp
#define szi32 pc+=sizeof(int32);
#define szarg pc+=sizeof(int32);
#define szlcl pc+=sizeof(int32);
#define szenv pc+=sizeof(int32);
#define szoff pc+=sizeof(int32);
#define szEs pc+=sizeof(int32);
#define szlit pc+=sizeof(int32);

#define instruction(Op, A1, Cmt)    \
      case Op:          \
  pc +=sizeof(uint16);      \
  sz##A1          \
    break;

#include "instructions.h"

#undef instruction
#undef sznOp
#undef sztos
#undef szi32
#undef szarg
#undef szlcl
#undef szenv
#undef szoff
#undef szEs
#undef szlit
      default:;
    }

    ins = ins->next;
  }

  // Check the labels
  ProcessTable(procLbl, mtd->labels, Null);
  // phase two: fixup labels
  ins = mtd->first;
  while (ins != Null) {
    fixup(ins);
    ins = ins->next;
  }
}

static assemInsPo newIns(mtdPo mtd, OpCode op) {
  assemInsPo ins = (assemInsPo) allocPool(insPool);

  if (mtd->first == Null)
    mtd->first = mtd->last = ins;
  else {
    mtd->last->next = ins;
    mtd->last = ins;
  }

  ins->op = op;
  ins->next = Null;

  return ins;
}

assemInsPo endIns(mtdPo mtd) {
  return mtd->last;
}

static lPo allocLbl(assemInsPo pc, char *name) {
  lPo label = (lPo) allocPool(lblPool);
  label->pc = pc;
  label->lbl = uniDuplicate(name);
  return label;
}

lPo newLbl(mtdPo mtd, char *lbl) {
  lPo label = hashGet(mtd->labels, lbl);
  if (label == Null) {
    label = allocLbl(Null, lbl);
    hashPut(mtd->labels, label->lbl, label);
  }
  return label;
}

lPo currLbl(mtdPo mtd, char *name) {
  lPo lbl = newLbl(mtd, name);
  defineLbl(mtd, lbl);

  return lbl;
}

void defineLbl(mtdPo mtd, lPo lbl) {
  assert(lbl->pc == Null);

  lbl->pc = newIns(mtd, label);
  lbl->pc->lbl = lbl;
}

retCode delLabel(void *n, void *r) {
  freePool(lblPool, (lPo) r);
  return Ok;
}

static void fixup_tos(assemInsPo ins) {
}

static void fixup_nOp(assemInsPo ins) {
}

static void fixup_i32(assemInsPo ins) {
}

static void fixup_arg(assemInsPo ins) {
}

static void fixup_lcl(assemInsPo ins) {
}

static void fixup_env(assemInsPo ins) {
}

static void fixup_off(assemInsPo ins) {
  assemInsPo tgt = ins->lbl->pc;
  ins->i = tgt->pc - ins->next->pc;
}

static void fixup_lit(assemInsPo ins) {
}

static void fixup_Es(assemInsPo ins) {
}

static void fixup(assemInsPo ins) {
  switch (ins->op) {
#undef instruction

#define instruction(Op, A1, Cmt)    \
    case Op:          \
      fixup_##A1(ins);      \
      break;

#include "instructions.h"

    case label:
    case frame:
    case illegalOp:;
#undef instruction
  }
}

retCode displayLabel(ioPo f, void *p, long width, long prec, logical alt) {
  lPo lbl = (lPo) p;
  return outMsg(f, "%U", lbl->lbl);
}

logical labelDefined(lPo lbl) {
  return (logical)(lbl->pc != Null);
}

static logical sameString(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, stringSig) == same)
    return (logical)(uniCmp(a1->value.txt, (char *) con) == same);
  else
    return False;
}

static logical sameInteger(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, integerSig) == same)
    return (logical)(a1->value.ix == *(int64 *) con);
  else
    return False;
}

static logical sameFloat(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, floatSig) == same)
    return (logical)(a1->value.dx == *(double *) con);
  else
    return False;
}

static logical sameMtd(constPo a1, char *sig, void *con) {
  mtdPo other = (mtdPo) con;

  return (logical)(a1->value.mtd == other);
}

static int32 findConstant(mtdPo mtd, char *sig, void *con) {
  listPo consts = mtd->constants;
  int32 count = 0;
  while (consts != nilList) {
    constPo cn = (constPo) head(consts);
    if (cn->same(cn, sig, con))
      return count;

    count++;
    consts = tail(consts);
  }

  return -1;
}

retCode showIntegerConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%l", cn->value.ix);
}

retCode encodeIntegerConstant(ioPo f, constPo c) {
  return encodeInt(f, c->value.ix);
}

int32 newIntegerConstant(mtdPo mtd, int64 ix) {
  int32 cx = findConstant(mtd, integerSig, &ix);

  if (cx < 0) {
    constPo conn = (constPo) allocPool(constPool);
    conn->sig = integerSig;
    conn->same = sameInteger;
    conn->show = showIntegerConstant;
    conn->encode = encodeIntegerConstant;
    conn->value.ix = ix;
    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32)listCount(mtd->constants) - 1;
  } else
    return cx;
}

retCode showFloatConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%ld", cn->value.dx);
}

retCode encodeFloatConstant(ioPo f, constPo c) {
  return encodeFlt(f, c->value.dx);
}

int32 newFloatConstant(mtdPo mtd, double dx) {
  int32 cx = findConstant(mtd, floatSig, &dx);

  if (cx < 0) {
    constPo conn = (constPo) allocPool(constPool);
    conn->sig = floatSig;
    conn->same = sameFloat;
    conn->show = showFloatConstant;
    conn->encode = encodeFloatConstant;
    conn->value.dx = dx;
    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32)(listCount(mtd->constants) - 1);
  } else
    return cx;
}

retCode showStringConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%U", cn->value.txt);
}

retCode encodeStringConstant(ioPo f, constPo c) {
  return encodeStr(f, c->value.txt);
}

int32 newStringConstant(mtdPo mtd, char *str) {
  int32 cx = findConstant(mtd, stringSig, str);

  if (cx < 0) {
    constPo conn = (constPo) allocPool(constPool);
    conn->sig = stringSig;
    conn->same = sameString;
    conn->show = showStringConstant;
    conn->encode = encodeStringConstant;
    conn->value.txt = uniDuplicate(str);
    mtd->constants = tack((objectPo) conn, mtd->constants);
    return (int32)(listCount(mtd->constants) - 1);
  } else
    return cx;
}

char *methodSignature(mtdPo mtd) {
  if (mtd->sig >= 0)
    return ((constPo) listNthElement(mtd->constants, mtd->sig))->value.txt;
  else
    return Null;
}

char *freeSignature(mtdPo mtd) {
  if (mtd->freeSig >= 0)
    return ((constPo) listNthElement(mtd->constants, mtd->freeSig))->value.txt;
  else
    return Null;
}

retCode showMtdConstant(ioPo f, constPo cn) {
  return outMsg(f, "[%U]\n", cn->value.mtd->name);
}

retCode encodeMtdConstant(ioPo f, constPo c) {
  return encodeStrct(f, c->value.mtd->name,c->value.mtd->arity);
}

int32 findMethod(mtdPo mtd, char *name, int arity) {
  pkgPo pkg = mtd->owner;

  mtdPo other = getPkgMethod(pkg, name);

  if (other != Null) {
    char *mtdSig = methodSignature(other);
    int32 cx = findConstant(mtd, mtdSig, name);

    if (cx >= 0)
      return cx;
    else {
      constPo conn = (constPo) allocPool(constPool);
      conn->sig = mtdSig;
      conn->same = sameMtd;
      conn->show = showMtdConstant;
      conn->encode = encodeMtdConstant;
      conn->value.mtd = other;
      mtd->constants = tack(O_OBJECT(conn), mtd->constants);
      return (int32)listCount(mtd->constants) - 1;
    }
  } else {
    other = defineMethod(pkg, False, name, arity, Null, Null);
    constPo conn = (constPo) allocPool(constPool);
    conn->sig = Null;
    conn->same = sameMtd;
    conn->show = showMtdConstant;
    conn->encode = encodeMtdConstant;
    conn->value.mtd = other;
    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32)listCount(mtd->constants) - 1;
  }
}

retCode showEscapeConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%U\n", cn->value.txt);
}

retCode encodeEscapeConstant(ioPo f, constPo c) {
  return encodeEscapeRef(f, c->value.txt);
}

int32 newEscapeConstant(mtdPo mtd, char *str) {
  int32 cx = findConstant(mtd, stringSig, str);

  if (cx < 0) {
    constPo conn = (constPo) allocPool(constPool);
    conn->sig = stringSig;
    conn->same = sameString;
    conn->show = showEscapeConstant;
    conn->encode = encodeEscapeConstant;
    conn->value.txt = uniDuplicate(str);
    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32)listCount(mtd->constants) - 1;
  } else
    return cx;
}

static assemInsPo asm_tos(mtdPo mtd, OpCode op) {
  assemInsPo ins = newIns(mtd, op);
  return ins;
}

static assemInsPo asm_nOp(mtdPo mtd, OpCode op) {
  assemInsPo ins = newIns(mtd, op);
  return ins;
}

static assemInsPo asm_i32(mtdPo mtd, OpCode op, int32 ix) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = ix;
  return ins;
}

static assemInsPo asm_arg(mtdPo mtd, OpCode op, int32 ix) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = ix;
  return ins;
}

static assemInsPo asm_lcl(mtdPo mtd, OpCode op, int32 ix) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = ix;
  return ins;
}

static assemInsPo asm_env(mtdPo mtd, OpCode op, int32 ix) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = ix;
  return ins;
}

static assemInsPo asm_lit(mtdPo mtd, OpCode op, int32 ix) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = ix;
  return ins;
}

static assemInsPo asm_Es(mtdPo mtd, OpCode op, int32 es) {
  assemInsPo ins = newIns(mtd, op);
  ins->i = es;
  return ins;
}

static assemInsPo asm_off(mtdPo mtd, OpCode op, lPo lbl) {
  assemInsPo ins = newIns(mtd, op);
  ins->lbl = lbl;
  return ins;
}

// Define the individual assembler instructions

#undef instruction

#define optos(X)
#define opnOp(X)
#define argtos(X)
#define argnOp(X)

#define opi32(X) ,int32 i##X
#define argi32(X) , i##X

#define oparg(X) ,int32 i##X
#define argarg(X) , i##X

#define oplcl(X) ,int32 i##X
#define arglcl(X) , i##X

#define openv(X) ,int32 i##X
#define argenv(X) , i##X

#define opoff(X) ,lPo l##X
#define argoff(X) ,l##X

#define oplit(X) ,int32 l##X
#define arglit(X) ,l##X

#define opEs(X) ,int32 f##X
#define argEs(X) , f##X

#define instruction(Op, A1, Cmt)    \
  assemInsPo A##Op(mtdPo mtd op##A1(1))    \
  {\
  return asm_##A1(mtd,Op arg##A1(1));\
  }

#include "instructions.h"

#undef instruction
#undef optos
#undef opnOp
#undef argtos
#undef argnOp
#undef opi32
#undef argi32
#undef oparg
#undef argarg
#undef oplcl
#undef arglcl
#undef openv
#undef argenv
#undef opoff
#undef argoff
#undef opEs
#undef argEs

int32 codeSize(mtdPo mtd) {
  int32 pc = 0;
  assemInsPo ins = mtd->first;

  while (ins != Null) {
    switch (ins->op) {

#undef instruction

#define sznOp
#define sztos
#define szi32 pc+=(sizeof(int32)/sizeof(uint16));
#define szarg pc+=(sizeof(int32)/sizeof(uint16));
#define szlcl pc+=(sizeof(int32)/sizeof(uint16));
#define szenv pc+=(sizeof(int32)/sizeof(uint16));
#define szoff pc+=(sizeof(int32)/sizeof(uint16));
#define szEs pc+=(sizeof(int32)/sizeof(uint16));
#define szlit pc+=(sizeof(int32)/sizeof(uint16));

#define instruction(Op, A1, Cmt)    \
      case Op:          \
  pc ++;          \
  sz##A1          \
    break;

#include "instructions.h"

#undef instruction
#undef sztos
#undef szi32
#undef szarg
#undef szlcl
#undef szenv
#undef szoff
#undef szEs
#undef szlit
      default:;
    }

    ins = ins->next;
  }
  return pc;
}

int64 poolCount(mtdPo mtd) {
  return listCount(mtd->constants);
}

constPo poolConstant(mtdPo mtd, int64 ix) {
  return (constPo) listNthElement(mtd->constants, ix);
}
