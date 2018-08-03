#include "config.h"
#include "ooio.h"

#include "engine.h"

retCode showInt(processPo P,ptrPo *tos) {
  termPo Tx = *(*tos)++;
  return outMsg(logFile, "%ld\n", (int64) Tx);
}

