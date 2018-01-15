#include "config.h"
#include "ooio.h"

#include "escape.h"
#include "signature.h"
#include "escodes.h"
#include "heap.h"

retCode showInt(ptrPo *tos) {
  termPo Tx = *(*tos)++;
  return outMsg(logFile, "%ld\n", (int64) Tx);
}

