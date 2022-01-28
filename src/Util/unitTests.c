//
// Created by Francis McCabe on 7/17/20.
//

#include <ooio.h>
#include <formioP.h>
#include <stdlib.h>
#include "unitTests.h"
#include "cmdOptions.h"
#include "version.h"      /* Version ID for the Star system */

int tests_run = 0;

logical debugUnitTests = False;

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

static retCode debugOption(char *option, logical enable) {
  char *c = option;

  while (*c) {
    switch (*c++) {
      case 'u':    /* debug unit tests */
        debugUnitTests = True;
        continue;

      default:
        return Error;
    }
  }

  return Ok;
}

static retCode debugOptHelp(ioPo out, char opt, char *usage) {
  return outMsg(out, "    -d|--debug <"
                     "u"
                     ">\n%_");
}

Option options[] = {
  {'d', "debug", hasArgument, "STAR_UNIT_OPTS", debugOption, "-d|--debug <flags>", debugOptHelp},};

int getUnitTestOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  return processOptions("(c) 2022 and beyind", argc, argv, options, NumberOf(options));
}

int main(int argc, char **argv) {
  if ((argc = getUnitTestOptions(argc, argv)) < 0) {
    exit(1);
  }
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
