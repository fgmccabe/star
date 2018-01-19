/*
 * Assembler for the Cafe machine code
 */

#include <assemP.h>
#include "assemP.h"
#include "formioP.h"
#include "stringBuffer.h"

#include "encoding.h"

static poolPo pkgPool;      /* pool of packages */
static poolPo mtdPool;      /* pool of method blocks */
static poolPo insPool;      /* pool of instructions */
static poolPo varPool;      /* pool of local variable records */
static poolPo lblPool;      /* pool of labels */
static poolPo impPool;      /* pool of imported package labels */

static retCode displayLabel(ioPo f, void *p, long width, long prec, logical alt);

static comparison localComp(localVarPo l1, localVarPo l2);
static integer localHash(localVarPo l);

static char stringSig[] = {strSig, 0};
static char integerSig[] = {intSig, 0};
static char floatSig[] = {fltSig, 0};
static char strctSig[] = {conSig, 0};

static void constInit(objectPo o, va_list *args);

ConstClassRec ConstClass = {
  {
    (classPo) &ObjectClass,
    "constant",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    constInit,
    sizeof(ConstRecord),
    NULL,
    NULL,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo constClass = (classPo) &ConstClass;

void constInit(objectPo o, va_list *args) {
  constPo c = O_CONST(o);

  c->con.sig = va_arg(*args, char *);
  c->con.same = va_arg(*args, constCmp);
  c->con.show = va_arg(*args, constDump);
  c->con.encode = va_arg(*args, constDump);
  c->con.value = *va_arg(*args, ConValue*);
}

constPo newConstant(char *sig, constCmp same, constDump show, constDump encode, ConValue *value) {
  return O_CONST(newObject(constClass, sig, same, show, encode, value));
}

void initAssem() {
  pkgPool = newPool(sizeof(AssemPackage), 3);
  mtdPool = newPool(sizeof(AssemMethod), 16);
  insPool = newPool(sizeof(AssemInstruction), 1024);
  varPool = newPool(sizeof(LocalVarRecord), 256);
  lblPool = newPool(sizeof(LabelRec), 128);
  impPool = newPool(sizeof(ImportRec), 16);

  installMsgProc('B', displayLabel);
  installMsgProc('P', dispPkgNm);
}

static retCode delLabel(void *n, void *r);
static retCode delMtd(strctPo nm, mtdPo mtd);
static retCode delImport(packagePo p, importPo q);

pkgPo newPkg(char *name, char *version) {
  pkgPo pkg = (pkgPo) allocPool(pkgPool);
  pkg->pkg = makePkg(name, version);
  pkg->methods = NewHash(16, (hashFun) strctHash, (compFun) strctComp, (destFun) delMtd);
  pkg->imports = NewHash(16, (hashFun) pkgHash, (compFun) compPkg, (destFun) delImport);
  return pkg;
}

retCode delImport(packagePo p, importPo q) {
  freePool(impPool, p);
  return Ok;
}

integer strctHash(strctPo st) {
  return uniHash(st->name) * 37 + st->arity;
}

comparison strctComp(strctPo st1, strctPo st2) {
  comparison comp = uniCmp(st1->name, st2->name);

  if (comp == same) {
    if (st1->arity < st2->arity)
      comp = smaller;
    else if (st1->arity > st2->arity)
      comp = bigger;
  }
  return comp;
}

static logical isRightMethod(objectPo d, void *cl) {
  StrctLbl *mtd = (StrctLbl *) cl;
  mtdPo m = (mtdPo) d;
  return (logical) (strctComp(mtd, &m->name) == same);
}

void addImport(pkgPo pkg, char *name, char *version, logical isPublic) {
  PackageRec p = makePkg(name, version);
  importPo ex = hashGet(pkg->imports, &p);

  if (ex == Null) {
    ex = (importPo) allocPool(impPool);
    ex->pkg = p;
    ex->isPublic = isPublic;
  }
  hashPut(pkg->imports, ex, ex);
}

mtdPo getPkgMethod(pkgPo pkg, const char *name, integer arity) {
  StrctLbl mtd = {.name =name, .arity=arity};
  return (mtdPo) hashGet(pkg->methods, &mtd);
}

static void defineSig(mtdPo mtd, char *sig);

mtdPo defineMethod(pkgPo pkg, char *name, integer arity, char *sig) {
  mtdPo existing = getPkgMethod(pkg, name, 0);

  if (existing == Null) {
    mtdPo mtd = (mtdPo) allocPool(mtdPool);
    mtd->name.name = uniDuplicate(name);
    mtd->name.arity = arity;
    mtd->labels = NewHash(16, (hashFun) uniHash, (compFun) uniCmp, delLabel);
    mtd->constants = nilList;
    mtd->first = mtd->last = Null;
    mtd->sig = -1;
    mtd->locals = NewHash(16, (hashFun) localHash, (compFun) localComp, NULL);

    newPrgConstant(mtd, name, arity);

    hashPut(pkg->methods, &mtd->name, mtd);

    if (sig != Null) {
      defineSig(mtd, sig);    /* should be the first constant */
    }

    return mtd;
  } else if (existing->sig < 0 && sig != Null) {
    defineSig(existing, sig);
  }

  return existing;
}

static retCode delMtd(strctPo nm, mtdPo mtd) {
  DelHash(mtd->locals);
  DelHash(mtd->frames);
  DelHash(mtd->labels);
  releaseList(mtd->constants);
  return Ok;
}

static void defineSig(mtdPo mtd, char *sig) {
  assert(mtd->sig < 0);

  mtd->sig = newStringConstant(mtd, sig);
  assert(mtd->sig == 1);
}

static assemInsPo asm_i32(mtdPo mtd, OpCode op, int32 ix);

void defineFrame(mtdPo mtd, char *sig) {
  asm_i32(mtd, Frame, newStringConstant(mtd, sig));
}

int32 frameCount(mtdPo mtd) {
  int32 cx = 0;
  assemInsPo ins = mtd->first;

  while (ins != Null) {
    if (ins->op == Frame)
      cx++;
    ins = ins->next;
  }
  return cx;
}

void defineLocal(mtdPo mtd, char *name, char *sig, lPo from, lPo to) {
  localVarPo var = (localVarPo) allocPool(varPool);
  var->name = uniDuplicate(name);
  var->sig = newStringConstant(mtd, sig);
  var->off = (int32) hashSize(mtd->locals);
  var->from = from;
  var->to = to;
  hashPut(mtd->locals, &var->name, var);
}

typedef struct {
  mtdPo mtd;
  const char *name;
  int32 count;
} Counter;

static logical lookforName(objectPo data, void *cl) {
  Counter *cnt = (Counter *) cl;
  localVarPo var = (localVarPo) data;
  char *varName;

  if (getStringConstant(cnt->mtd, &varName) == Ok) {
    if (uniCmp(varName, cnt->name) == same)
      return True;
  }

  cnt->count++;
  return False;
}

int32 findLocal(mtdPo mtd, const char *name) {
  Counter cnt = {.mtd=mtd, .name=name, .count=0};

  localVarPo lclVar = (localVarPo) hashGet(mtd->locals, (void *) name);
  if (lclVar != Null)
    return lclVar->off;
  else
    return -1;
}

comparison localComp(localVarPo l1, localVarPo l2) {
  return uniCmp(l1->name, l2->name);
}

static integer localHash(localVarPo l) {
  return uniHash(l->name);
}

retCode delFunction(pkgPo pkg, mtdPo mtd) {
  hashRemove(pkg->methods, &mtd->name);

  DelHash(mtd->labels);
  DelHash(mtd->frames);
  DelHash(mtd->locals);

  assemInsPo ins = mtd->first;
  while (ins != Null) {
    assemInsPo next = ins->next;
    freePool(insPool, ins);
    ins = next;
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
    case illegalOp:;
#undef instruction
  }
}

retCode displayLabel(ioPo f, void *p, long width, long prec, logical alt) {
  lPo lbl = (lPo) p;
  return outMsg(f, "%U", lbl->lbl);
}

logical labelDefined(lPo lbl) {
  return (logical) (lbl->pc != Null);
}

char *lblName(lPo lbl) {
  return lbl->lbl;
}

static logical sameString(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, stringSig) == same)
    return (logical) (uniCmp(a1->con.value.txt, (char *) con) == same);
  else
    return False;
}

static logical sameInteger(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, integerSig) == same)
    return (logical) (a1->con.value.ix == *(int64 *) con);
  else
    return False;
}

static logical sameFloat(constPo a1, char *sig, void *con) {
  if (uniCmp(sig, floatSig) == same)
    return (logical) (a1->con.value.dx == *(double *) con);
  else
    return False;
}

static logical sameMtd(constPo a1, char *sig, void *con) {
  mtdPo other = (mtdPo) con;

  return (logical) (a1->con.value.mtd == other);
}

static logical isStruct(strctPo str1, strctPo str2) {
  return (logical) (uniCmp(str1->name, str2->name) == same && str1->arity == str2->arity);
}

static logical sameStruct(constPo a1, char *sig, void *con) {
  return (logical) (uniCmp(a1->con.sig, sig) == same && isStruct(&a1->con.value.strct, (strctPo) con));
}

static int32 findConstant(mtdPo mtd, char *sig, void *con) {
  listPo consts = mtd->constants;
  int32 count = 0;
  while (consts != nilList) {
    constPo cn = (constPo) head(consts);
    if (cn->con.same(cn, sig, con))
      return count;

    count++;
    consts = tail(consts);
  }

  return -1;
}

retCode showIntegerConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%d", cn->con.value.ix);
}

retCode encodeIntegerConstant(ioPo f, constPo c) {
  return encodeInt(f, c->con.value.ix);
}

int32 newIntegerConstant(mtdPo mtd, int64 ix) {
  int32 cx = findConstant(mtd, integerSig, &ix);

  if (cx < 0) {
    ConValue value = {.ix=ix};

    constPo conn = newConstant(integerSig, sameInteger, showIntegerConstant, encodeIntegerConstant, &value);

    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32) listCount(mtd->constants) - 1;
  } else
    return cx;
}

retCode showFloatConstant(ioPo f, constPo cn) {
  return outMsg(f, "#%ld", cn->con.value.dx);
}

retCode encodeFloatConstant(ioPo f, constPo c) {
  return encodeFlt(f, c->con.value.dx);
}

int32 newFloatConstant(mtdPo mtd, double dx) {
  int32 cx = findConstant(mtd, floatSig, &dx);

  if (cx < 0) {
    ConValue value = {.dx=dx};

    constPo conn = newConstant(floatSig, sameFloat, showFloatConstant, encodeFloatConstant, &value);

    mtd->constants = tack(O_OBJECT(conn), mtd->constants);
    return (int32) (listCount(mtd->constants) - 1);
  } else
    return cx;
}

retCode showStringConstant(ioPo f, constPo cn) {
  return outMsg(f, "#\"%Q\"", cn->con.value.txt);
}

retCode encodeStringConstant(ioPo f, constPo c) {
  return encodeStr(f, c->con.value.txt, uniStrLen(c->con.value.txt));
}

int32 newStringConstant(mtdPo mtd, char *str) {
  int32 cx = findConstant(mtd, stringSig, str);

  if (cx < 0) {
    ConValue value = {.txt=uniDuplicate(str)};

    constPo conn = newConstant(stringSig, sameString, showStringConstant, encodeStringConstant, &value);

    mtd->constants = tack((objectPo) conn, mtd->constants);
    return (int32) (listCount(mtd->constants) - 1);
  } else
    return cx;
}

retCode getStringConstant(mtdPo mtd, char **name) {
  int32 cx = findConstant(mtd, stringSig, name);
  if (cx < 0)
    return Fail;
  else
    return Ok;
}

retCode showStructConstant(ioPo f, constPo cn) {
  return outMsg(f, "%s:%d", cn->con.value.strct.name, cn->con.value.strct.arity);
}

retCode encodeStructConstant(ioPo f, constPo c) {
  return encodeStrct(f, (char *) c->con.value.strct.name, c->con.value.strct.arity);
}

retCode showPrgConstant(ioPo f, constPo cn) {
  return outMsg(f, "%s/%d", cn->con.value.strct.name, cn->con.value.strct.arity);
}

retCode encodePrgConstant(ioPo f, constPo c) {
  return encodePrg(f, (char *) c->con.value.strct.name, c->con.value.strct.arity);
}

int32 newStrctConstant(mtdPo mtd, char *str, integer ar) {
  StrctLbl S = {str, ar};

  int32 cx = findConstant(mtd, strctSig, &S);

  if (cx < 0) {
    ConValue value = {.strct={.name=uniDuplicate(str), .arity=ar}};

    constPo conn = newConstant(strctSig, sameStruct, showStructConstant, encodeStructConstant, &value);

    mtd->constants = tack((objectPo) conn, mtd->constants);
    return (int32) (listCount(mtd->constants) - 1);
  } else
    return cx;
}

int32 newPrgConstant(mtdPo mtd, char *str, integer ar) {
  StrctLbl S = {str, ar};

  int32 cx = findConstant(mtd, strctSig, &S);

  if (cx < 0) {
    ConValue value = {.strct={.name=uniDuplicate(str), .arity=ar}};

    constPo conn = newConstant(strctSig, sameStruct, showPrgConstant, encodePrgConstant, &value);

    mtd->constants = tack((objectPo) conn, mtd->constants);
    return (int32) (listCount(mtd->constants) - 1);
  } else
    return cx;
}

char *methodSignature(mtdPo mtd) {
  if (mtd->sig >= 0)
    return ((constPo) listNthElement(mtd->constants, mtd->sig))->con.value.txt;
  else
    return Null;
}

int32 newEscapeConstant(mtdPo mtd, char *str) {
  return newStringConstant(mtd, str);
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
  retCode A##Op(mtdPo mtd op##A1(1))    \
  {\
    asm_##A1(mtd,Op arg##A1(1));\
    return Ok;\
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
#undef szoff
#undef szEs
#undef szlit
#undef sznOp
      default:;
    }

    ins = ins->next;
  }
  return pc;
}

int32 codeCount(mtdPo mtd) {
  int32 count = 0;
  assemInsPo ins = mtd->first;

  while (ins != Null) {
    switch (ins->op) {
      case label:
        break;
      default:
        count++;
    }

    ins = ins->next;
  }

  return count;
}

int64 poolCount(mtdPo mtd) {
  return listCount(mtd->constants);
}

constPo poolConstant(mtdPo mtd, int64 ix) {
  return (constPo) listNthElement(mtd->constants, ix);
}

static retCode assembleIns(mtdPo mtd, bufferPo bfr) {
  assemInsPo ins = mtd->first;
  retCode ret = Ok;

  while (ret == Ok && ins != Null) {
    switch (ins->op) {

#undef instruction

#define sznOp
#define szi32 if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);
#define szarg if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);
#define szlcl if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);
#define szoff if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);
#define szEs if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);
#define szlit if(ret==Ok)ret = encodeInt(O_IO(bfr),ins->i);

#define instruction(Op, A1, Cmt)    \
      case Op:          \
        ret = outByte(O_IO(bfr),Op);\
  sz##A1          \
    break;

#include "instructions.h"

#undef instruction
#undef sztos
#undef szi32
#undef szarg
#undef szlcl
#undef szoff
#undef szEs
#undef szlit
#undef sznOp
      default:;
    }

    ins = ins->next;
  }
  return ret;
}
