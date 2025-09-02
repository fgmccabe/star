//
// Created by Francis McCabe on 8/23/25.
//

#include "ooio.h"
#include "abort.h"

#include <stdlib.h>

static char *exitCodes[] = {
  "normal exit",
  "failing exit",
  "error",
  "out of memory",
  "tried to execute undefined code",
  "something wrong with fiber",
  "something wrong with single assignment variables",
  "something wrong with assignment",
  "something wrong with a special method",
  "tried an invalid operation",
  "execution aborted"
};

void star_exit(ExitCode code) {
  if (code != successCode)
    outMsg(logFile, "Terminating with code %s (%d)\n", (code <= abortCode ? exitCodes[code] : "unknown exit code"),
           code);

  exit(code);
}
