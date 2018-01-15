/*
 * Handle compilation of arithmetic expressions
 */

#include "compiler.h"
#include "dict.h"
#include "compile.h"
#include "esc.h"
#include "meta.h"
#include "codegen.h"

static retCode intPlus(mtdCxtPo mtd, int arity, exitPo exit, int stkDepth, int *retDepth) {
  if (arity != 2 || stkDepth > MAX_OPERANDS || stkDepth < 2)
    return Error;
  else {
    assemInsPo code = methodCode(mtd);
    retCode ret = updateMtdIns(mtd, AAddI(code), code);
    *retDepth = stkDepth - 1;
    return ret;
  }
}

retCode initArith() {
  declareInline("_int_plus", intPlus);
  return Ok;
}
