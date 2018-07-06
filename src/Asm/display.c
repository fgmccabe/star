/*
 * Display an assembled fragment
 */
#include "assemP.h"
#include "starOptions.h"
#include "utils.h"

#include <ooio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "formioP.h"

static retCode disp_tOs(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s\n", op);
}

static retCode disp_nOp(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s\n", op);
}

static retCode disp_i32(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s %d\n", op, ins->i);
}

static retCode disp_arg(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s a[%d]\n", op, ins->i);
}

static retCode disp_lcl(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s l[%d]\n", op, ins->i);
}

static retCode disp_lcs(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s l[%d]\n", op, ins->i);
}

static retCode disp_env(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s e[%d]\n", op, ins->i);
}

static retCode disp_off(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  return outMsg(f, "%s %B\n", op, ins->lbl);
}

static retCode showConstant(ioPo f, mtdPo mtd, int64 ix) {
  constPo con = poolConstant(mtd, ix);
  if (con != Null)
    return con->con.show(f, con);
  else
    return outMsg(f, " <bad constant %d>", ix);
}

static retCode disp_lit(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  tryRet(outMsg(f, "%s ", op));
  tryRet(showConstant(f, mtd, ins->i));
  return outMsg(f, "\n");
}

static retCode disp_Es(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  tryRet(outMsg(f, "%s ", op));
  tryRet(showConstant(f, mtd, ins->i));
  return outMsg(f, "\n");
}

static retCode disp_glb(ioPo f, mtdPo mtd, assemInsPo ins, char *op) {
  tryRet(outMsg(f, "%s ", op));
  tryRet(showConstant(f, mtd, ins->i));
  return outMsg(f, "\n");
}

retCode dumpIns(ioPo f, mtdPo mtd, assemInsPo ins) {
  outMsg(f, "0x%x: ", ins->pc);

  switch (ins->op) {
#undef instruction

#define instruction(Op, A1, Dl, Cmt) \
    case Op:\
      return disp_##A1(f,mtd,ins,#Op);

#include "instructions.h"

    case label:
      return outMsg(f, "%B:\n", ins->lbl);

    default:
      return Error;
  }
}

typedef struct {
  ioPo out;
  mtdPo mtd;
} MtdData;

static retCode showLocal(void *nm, void *r, void *cl) {
  MtdData *data = (MtdData *) cl;
  localVarPo vr = (localVarPo) r;

  constPo c = poolConstant(data->mtd, vr->sig);

  if (c != Null) {
    char *sig = c->con.value.txt;

    outMsg(data->out, "%s:%s [%d]", vr->name, sig, vr->off);

    if (vr->from->pc != Null && vr->to->pc != Null)
      return outMsg(data->out, " (0x%x-0x%x)\n", vr->from->pc->pc, vr->to->pc->pc);
    else
      return outMsg(data->out, "\n");
  }

  return Error;
}

retCode dumpMethod(void *n, void *r, void *c) {
  mtdPo mtd = (mtdPo) r;
  ioPo io = (ioPo) c;
  outMsg(io, "method %s/%d: %s\n", mtd->name.name, mtd->name.arity, methodSignature(mtd));

  assemInsPo ins = mtd->first;
  while (ins != Null) {
    tryRet(dumpIns(io, mtd, ins));
    ins = ins->next;
  }

  MtdData data = {.out = io, .mtd = mtd};

  ProcessTable(showLocal, mtd->locals, &data);

  return Ok;
}

retCode showImport(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  importPo imp = (importPo)r;

  return outMsg(out, "%simport %P\n",imp->isPublic?"public ":"",&imp->pkg);
}

void dumpPkgCode(pkPo pkg) {
  outMsg(logFile, "Package %P:\n", &pkg->pkg);
  ProcessTable(showImport, pkg->imports, logFile);
  ProcessTable(dumpMethod, pkg->methods, logFile);
  flushOut();
}


