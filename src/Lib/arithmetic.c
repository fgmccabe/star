//
// Created by Francis McCabe on 1/6/18.
//

#include "arithmetic.h"

retCode _int_plus(ptrPo *tos) {
  termPo Tx = *(*tos)++;
  return outMsg(logFile, "%ld\n", (int64) Tx);
}

