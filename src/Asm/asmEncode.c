/*
 * encode a bunch of methods into object file format
 */
#include "config.h"
#include <ooio.h>
#include "encoding.h"
#include "assemP.h"
#include <assert.h>

static retCode encodeIns(ioPo out, assemInsPo ins);

static retCode encodeMethod(ioPo out, mtdPo mtd);

static retCode encodeFrame(ioPo out, mtdPo mtd, assemInsPo ins);
static retCode encodeLocal(void *n,void *r,void *cl);


typedef struct {
  ioPo out;
  mtdPo mtd;
} MtdData;

static retCode encMtd(void *n, void *r, void *cl) {
  ioPo out = (ioPo)cl;
  mtdPo mtd = (mtdPo)r;

  return encodeMethod(out, mtd);
}

retCode encodePkg(ioPo out, pkgPo pkg) {
  MtdData data = {.out = out,};
  return ProcessTable(encMtd,pkg->methods,out);
}

static retCode encodeConstant(ioPo out, constPo con) {
  return con->con.encode(out, con);
}

static retCode encodeTplCount(ioPo out, integer cnt) {
  char buff[MAXFILELEN];
  strMsg(buff, NumberOf(buff), "()%d", cnt);
  tryRet(outChar(out, dtaTrm));
  return encodeStrct(out, buff, cnt);
}

retCode encodeMethod(ioPo out, mtdPo mtd) {
  assert(mtd->sig == 1);

  tryRet(encodeStrct(out, "#code", 6));
  tryRet(encodeStrct(out, (char*)mtd->name.name, mtd->name.arity)); /* signal tag w/name */

  tryRet(encodeInt(out, mtd->sig));        /* Constant id for the type signature */

  tryRet(encodeTplCount(out, codeCount(mtd)));
  for (assemInsPo pc = mtd->first; pc != Null; pc = pc->next) {
    tryRet(encodeIns(out, pc));
  }

  tryRet(encodeTplCount(out, poolCount(mtd)));
  for (listPo con = mtd->constants; con != nilList; con = tail(con)) {
    tryRet(encodeConstant(out, (constPo) head(con)));
  }

  tryRet(encodeTplCount(out, frameCount(mtd)));
  for (assemInsPo pc = mtd->first; pc != Null; pc = pc->next) {
    tryRet(encodeFrame(out, mtd, pc));
  }

  tryRet(encodeTplCount(out, localCount(mtd))); /* Number of local var records */

  MtdData data = {.mtd = mtd, .out = out};
  ProcessTable(encodeLocal,mtd->locals,&data);


  return Ok;
}

retCode encodeFrame(ioPo out, mtdPo mtd, assemInsPo ins) {
  if (ins->op == frame) {
    tryRet(encodeInt(out, ins->i));
    tryRet(encodeInt(out, ins->pc / sizeof(uint16)));
  }
  return Ok;
}

retCode encodeLocal(void *n,void *r,void *cl){
  MtdData *data = (MtdData*)cl;
  ioPo out = data->out;
  localVarPo lcl = (localVarPo)r;

  tryRet(encodeStr(out, lcl->name));
  tryRet(encodeInt(out, lcl->sig));
  tryRet(encodeInt(out, lcl->off));
  tryRet(encodeInt(out, lcl->from->pc->pc / sizeof(uint16)));
  return encodeInt(out, lcl->to->pc->pc / sizeof(uint16));
}

static retCode enc_nOp(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->op);
}

static retCode enc_i32(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_arg(ioPo out, assemInsPo ins) {
  tryRet(encodeTplCount(out, 2));
  tryRet(encodeInt(out,ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_lcl(ioPo out, assemInsPo ins) {
  tryRet(encodeTplCount(out, 2));
  tryRet(encodeInt(out,ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_off(ioPo out, assemInsPo ins) {
  tryRet(encodeTplCount(out, 2));
  tryRet(encodeInt(out,ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_lit(ioPo out, assemInsPo ins) {
  tryRet(encodeTplCount(out, 2));
  tryRet(encodeInt(out,ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_Es(ioPo out, assemInsPo ins) {
  tryRet(encodeTplCount(out, 2));
  tryRet(encodeInt(out,ins->op));
  return encodeInt(out, ins->i);
}

retCode encodeIns(ioPo out, assemInsPo ins) {
  switch (ins->op) {
#undef instruction
#define instruction(Op, A1, Cmt)    \
    case Op:          \
      return enc_##A1(out,ins);

#include "instructions.h"

    case label:
      return Ok;
    case frame:
      tryRet(encodeTplCount(out, 2));
      tryRet(encodeInt(out,ins->op));
      return encodeInt(out, ins->i);
    default:
      return Error;
  }
}
