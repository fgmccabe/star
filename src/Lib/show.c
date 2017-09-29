#include "config.h"
#include "ooio.h"

#include "escape.h"
#include "signature.h"
#include "libNames.h"
#include "escodes.h"
#include "heap.h"

static ptrPo showInt(ptrPo tos) {
  termPo Tx = *tos;
  outMsg(logFile, "%ld\n", (int64) Tx);
  return tos;
}

