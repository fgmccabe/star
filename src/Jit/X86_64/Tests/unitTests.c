//
// Created by Francis McCabe on 7/17/20.
//

#include <ooio.h>
#include <stdlib.h>
#include <formioP.h>
#include "unitTests.h"
#include "x86_64P.h"

int tests_run = 0;

x64CtxPo setupCtx(x64CtxPo ctx) {
  ctx->bytes = malloc(1024);
  ctx->size = 1024;
  ctx->pc = 0;
  return ctx;
}

retCode cmpBytes(u8 *lft, u8 *rgt, integer count) {
  for (integer ix = 0; ix < count; ix++)
    if (lft[ix] != rgt[ix]) {
      logMsg(logFile, "byte at %d is 0x%x, should be 0x%x", ix, rgt[ix], lft[ix]);
      logMsg(logFile, "actual bytes: %.*X",count,rgt);
      return Error;
    }

  return Ok;
}

retCode checkResult(u8 *src, integer srcLen, x64CtxPo ctx) {
  if (ctx->pc != srcLen) {
    logMsg(logFile, "%d bytes expected, %d bytes generated", srcLen, ctx->pc);
    logMsg(logFile, "actual bytes: %.*X",ctx->pc,ctx->bytes);
    return Error;
  } else
    return cmpBytes(src, ctx->bytes, srcLen);
}

retCode run_test(tester test) {
  tests_run++;
  return test();
}

retCode showByteSeq(ioPo f, void *data, long depth, long precision, logical alt) {
  byte *txt = (byte *) data;
  retCode ret = Ok;
  char *sep = "";

  for(integer ix=0;ix<precision && ret==Ok;ix++) {
    ret = outMsg(f, "%s0x%2x", sep, (txt[ix]&0xffu));
    sep = ", ";
  }

  return ret;
}

int main(int argc, char **argv) {
  initLogfile("-");
  installMsgProc('X', showByteSeq);

  retCode ret = all_tests();

  if (ret == Ok) {
    logMsg(logFile, "all %d tests passed", tests_run);
  } else {
    logMsg(logFile, "test %d failed", tests_run);
  }
  return ret;
}
