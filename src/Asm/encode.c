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
static retCode encodeLocal(ioPo out, mtdPo mtd, localVarPo lcl);

static retCode encMtd(void *r, void *c) {
  ioPo out = (ioPo) c;
  mtdPo mtd = (mtdPo) r;

  return encodeMethod(out, mtd);
}

retCode encodePkg(ioPo out, pkgPo pkg) {
  tryRet(encodeInt(out, listCount(pkg->methods), trmDct));  /* pkg dictionary */

  return processList(pkg->methods, encMtd, out); /* process all the methods */
}

static retCode encodeConstant(ioPo out, constPo con) {
  tryRet(encodeStr(out, con->sig));
  return con->encode(out, con);
}

retCode encodeMethod(ioPo out, mtdPo mtd) {
  assert(mtd->sig == 0 && mtd->freeSig == 1);

  tryRet(encodeStr(out, mtd->name, trmTag)); /* signal tag w/name */

  tryRet(outByte(out, trmCde));    /* signal a code block */
  tryRet(encodeInt(out, mtd->sig));        /* Constant id for the type signature */
  tryRet(encodeInt(out, mtd->freeSig));    /* Which constant is the free signature? */
  tryRet(encodeInt(out, poolCount(mtd)));
  tryRet(encodeInt(out, frameCount(mtd))); /* Number of frame records */
  tryRet(encodeInt(out, localCount(mtd))); /* Number of local var records */
  tryRet(encodeInt(out, codeSize(mtd)));

  for (listPo con = mtd->constants; con != emptyList; con = tail(con)) {
    tryRet(encodeConstant(out, (constPo) head(con)));
  }

  for (assemInsPo pc = mtd->first; pc != Null; pc = pc->next) {
    tryRet(encodeIns(out, pc));
  }

  for (assemInsPo pc = mtd->first; pc != Null; pc = pc->next) {
    tryRet(encodeFrame(out, mtd, pc));
  }

  for (listPo lcl = mtd->locals; lcl != emptyList; lcl = tail(lcl)) {
    tryRet(encodeLocal(out, mtd, (localVarPo) head(lcl)));
  }
  return Ok;
}

retCode encodeFrame(ioPo out, mtdPo mtd, assemInsPo ins) {
  if (ins->op == DefineFrame) {
    tryRet(encodeInt(out, ins->i));
    tryRet(encodeInt(out, ins->pc / sizeof(uint16)));
  }
  return Ok;
}

retCode encodeLocal(ioPo out, mtdPo mtd, localVarPo lcl) {
  tryRet(encodeInt(out, lcl->name));
  tryRet(encodeInt(out, lcl->sig));
  tryRet(encodeInt(out, lcl->off));
  tryRet(encodeInt(out, lcl->from->pc->pc / sizeof(uint16)));
  return encodeInt(out, lcl->to->pc->pc / sizeof(uint16));
}

static retCode enc_tos(ioPo out, assemInsPo ins) {
  return Ok;
}

static retCode enc_nOp(ioPo out, assemInsPo ins) {
  return Ok;
}

static retCode enc_i32(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_arg(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_lcl(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_env(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_off(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_lit(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

static retCode enc_Es(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->i);
}

retCode encodeIns(ioPo out, assemInsPo ins) {
  switch (ins->op) {
#undef instruction
#define instruction(Op, A1, A2, Cmt)    \
    case Op:          \
      tryRet(outByte(out,Op));      \
      return enc_##A1(out,ins);

#include "instructions.h"

    case label:
    case DefineFrame:
      return Ok;
    default:
      return Error;
  }
}

retCode encodeInt(ioPo out, int64 ix) {
  return outMsg(out, "x%ld", ix);
}

retCode encodeStr(ioPo out,char *str){
  tryRet(outChar(out,'s'));
  return encodeTxt(out,str);
}

retCode encodeTxt(ioPo out, char *str) {
  codePoint delim = findDelim(str, "'\"|/%");
  integer len = uniStrLen(str);
  codePoint ch;
  retCode ret = outChar(out, delim);

  for (integer pos = 0; ret == Ok && pos < len;) {
    ch = nextCodePoint(str, &pos, len);
    if (ch == delim || ch == '\\') {
      ret = outChar(out, '\\');
    }
    if (ret == Ok)
      ret = outChar(out, ch);
  }

  if (ret == Ok)
    ret = outChar(out, delim);
  return ret;
}

retCode encodeFlt(ioPo out, double dx) {
  return outMsg(out, "d%f", dx);
}

retCode encodePrg(ioPo out, char *sx, integer ar) {
  tryRet(outChar(out, 'p'));
  tryRet(outInt(out, ar));
  return encodeTxt(out, sx);
}

retCode encodeStrct(ioPo out, char *sx, integer ar) {
  tryRet(outChar(out, 'o'));
  tryRet(outInt(out, ar));
  return encodeTxt(out, sx);
}
