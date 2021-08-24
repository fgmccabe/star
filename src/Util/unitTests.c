//
// Created by Francis McCabe on 7/17/20.
//

#include <ooio.h>
#include <formioP.h>
#include "unitTests.h"

int tests_run = 0;

retCode cmpBytes(byte *lft, byte *rgt, integer count) {
  for (integer ix = 0; ix < count; ix++)
    if (lft[ix] != rgt[ix]) {
      logMsg(logFile, "byte at %d is 0x%x, should be 0x%x", ix, rgt[ix], lft[ix]);
      logMsg(logFile, "actual bytes: %.*X", count, rgt);
      return Error;
    }

  return Ok;
}

retCode run_test(tester tst) {
  tests_run++;
  return tst();
}

retCode showByteSeq(ioPo f, void *data, long depth, long precision, logical alt) {
  byte *txt = (byte *) data;
  retCode ret = Ok;
  char *sep = "";

  for (integer ix = 0; ix < precision && ret == Ok; ix++) {
    ret = outMsg(f, "%s0x%2x", sep, (txt[ix] & 0xffu));
    sep = ", ";
  }

  return ret;
}

retCode checkReslt(int64 test, int64 verify, char *msg) {
  if (test != verify) {
    logMsg(logFile, "Test %msg failed, expected %ld, got %ld", msg, verify, test);
    return Error;
  } else
    return Ok;
}

int main(int argc, char **argv) {
  initLogfile("-");
  installMsgProc('X', showByteSeq);

  tests_run = 0;

  retCode ret = all_tests();

  if (ret == Ok) {
    logMsg(logFile, "all %d tests passed", tests_run);
  } else {
    logMsg(logFile, "test %d failed", tests_run);
  }
  return ret;
}
