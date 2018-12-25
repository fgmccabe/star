/*
 * encode a bunch of methods into object file format
 */
#include "config.h"
#include <ooio.h>
#include "stringBuffer.h"
#include "encoding.h"
#include "assemP.h"
#include <assert.h>

static retCode encodeIns(ioPo out, assemInsPo ins);

static retCode encodeMethod(ioPo out, mtdPo mtd);

static retCode encodeFrame(ioPo out, mtdPo mtd, assemInsPo ins);
static retCode encodeLocal(void *n, void *r, void *cl);
static retCode encodeTplCount(ioPo out, integer cnt);

typedef struct {
  ioPo out;
  mtdPo mtd;
} MtdData;

static retCode encMtd(void *n, void *r, void *cl) {
  ioPo out = (ioPo) cl;
  mtdPo mtd = (mtdPo) r;

  return encodeMethod(out, mtd);
}

static retCode encodeMtds(ioPo out, hashPo mtds) {
  tryRet(encodeTplCount(out, hashSize(mtds)));
  return ProcessTable(encMtd, mtds, out);
}

static retCode encodePkgName(ioPo out, packagePo pkg) {
  tryRet(encodeStrct(out, "pkg", 2));
  tryRet(encodeStr(out, pkg->packageName, uniStrLen(pkg->packageName)));
  if (uniIsLit(pkg->version, "*")) {
    return encodeEnum(out, "*");
  } else {
    tryRet(encodeStrct(out, "v", 1));
    return encodeStr(out, pkg->version, uniStrLen(pkg->version));
  }
}

static retCode encImport(void *n, void *r, void *cl) {
  ioPo out = (ioPo) cl;
  importPo i = (importPo) r;

  tryRet(encodeStrct(out, "import", 2));
  tryRet(encodeEnum(out, i->isPublic ? "public" : "private"));
  tryRet(encodePkgName(out, &i->pkg));
  return Ok;
}

static retCode encodeImports(ioPo out, hashPo imports) {
  tryRet(encodeTplCount(out, hashSize(imports)));
  return ProcessTable(encImport, imports, out);
}

retCode encodePkg(ioPo out, pkPo pkg) {
  bufferPo str = newStringBuffer();
  tryRet(encodeTplCount(O_IO(str), 4));
  tryRet(encodePkgName(O_IO(str), &pkg->pkg));
  tryRet(encodeStr(O_IO(str), pkg->signature, uniStrLen(pkg->signature)));
  tryRet(encodeImports(O_IO(str), pkg->imports));
  tryRet(encodeMtds(O_IO(str), pkg->methods));

  rewindBuffer(str);
  integer buffLen;
  char *buffer = getTextFromBuffer(str, &buffLen);
  tryRet(encodeStr(out, buffer, buffLen));
  closeFile(O_IO(str));
  return Ok;
}

static retCode encodeConstant(ioPo out, constPo con) {
  return con->con.encode(out, con);
}

static retCode encodeLine(ioPo out,linePo lne);

static retCode encodeTplCount(ioPo out, integer cnt) {
  char buff[MAXFILELEN];
  strMsg(buff, NumberOf(buff), "()%d", cnt);
  tryRet(outChar(out, dtaTrm));
  return encodeStrct(out, buff, cnt);
}

retCode encodeMethod(ioPo out, mtdPo mtd) {
  tryRet(encodeCons(out, 7));
  tryRet(encodeTplCount(out, 7));
  tryRet(encodeStrct(out, (char *) mtd->name.name, mtd->name.arity)); /* signal tag w/name */

  tryRet(encodeInt(out, mtd->sig));        /* Constant id for the type signature */

  tryRet(encodeInt(out, mtd->lclCount));   // how many locals

  tryRet(encodeTplCount(out, codeSize(mtd)));
  for (assemInsPo pc = mtd->first; pc != Null; pc = pc->next) {
    tryRet(encodeIns(out, pc));
  }

  tryRet(encodeTplCount(out, poolCount(mtd)));
  for (consPo con = mtd->constants; con != nilList; con = tail(con)) {
    tryRet(encodeConstant(out, (constPo) head(con)));
  }

  tryRet(encodeTplCount(out, localCount(mtd))); /* Number of local var records */

  MtdData data = {.mtd = mtd, .out = out};
  ProcessTable(encodeLocal, mtd->locals, &data);

  tryRet(encodeTplCount(out, lineCount(mtd))); /* Number of line records */
  for(consPo lne=mtd->lines;lne!=nilList;lne=tail(lne)){
    tryRet(encodeLine(out,O_LINE(head(lne))));
  }

  return Ok;
}

retCode encodeFrame(ioPo out, mtdPo mtd, assemInsPo ins) {
  if (ins->op == Frame) {
    tryRet(encodeInt(out, ins->i));
    tryRet(encodeInt(out, ins->pc / sizeof(uint16)));
  }
  return Ok;
}

retCode encodeLocal(void *n, void *r, void *cl) {
  MtdData *data = (MtdData *) cl;
  ioPo out = data->out;
  localVarPo vr = (localVarPo) r;

  tryRet(encodeStr(out, vr->name, uniStrLen(vr->name)));
  tryRet(encodeInt(out, vr->sig));
  tryRet(encodeInt(out, vr->off));
  tryRet(encodeInt(out, vr->from->pc->pc / sizeof(uint16)));
  return encodeInt(out, vr->to->pc->pc / sizeof(uint16));
}

retCode encodeLine(ioPo out,linePo lne) {
  tryRet(encodeTplCount(out,2));

  tryRet(encodeInt(out,lne->line.locRef));
  return encodeInt(out,lne->line.lbl->pc->pc);
}

static retCode enc_nOp(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->op);
}

static retCode enc_tOs(ioPo out, assemInsPo ins) {
  return encodeInt(out, ins->op);
}

static retCode enc_art(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}


static retCode enc_i32(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_arg(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_lcl(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_lcs(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_off(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_lit(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_Es(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

static retCode enc_glb(ioPo out, assemInsPo ins) {
  tryRet(encodeInt(out, ins->op));
  return encodeInt(out, ins->i);
}

retCode encodeIns(ioPo out, assemInsPo ins) {
  switch (ins->op) {
#undef instruction
#define instruction(Op, A1, Dl, Cmt)    \
    case Op:          \
      return enc_##A1(out,ins);

#include "instructions.h"

    case label:
      return Ok;
    default:
      return Error;
  }
}
