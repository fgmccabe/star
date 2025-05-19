//
// Created by Francis McCabe on 5/18/25.
//

#include "disass.h"

static uint32 bits(uint32 word, uint8 from, uint8 count) {
  return (word >> from) & ((1u << count) - 1u);
}

static uint32 maskedBits(uint32 word, uint8 from, uint8 count, uint32 mask) {
  return bits(word, from, count) & mask;
}

static uint8 op1(uint32 word) {
  return (uint8) bits(word, 24, 8);
}

static logical isReserved(uint32 word) {
  return ((word & (1u << 31)) == 0) && bits(word, 29, 2) == 0x00;
}

static logical isSME(uint32 word) {
  if (bits(word, 31, 1) == 0x1 && bits(word, 25, 4) == 0) {
    switch (bits(word, 29, 2)) {
      case 0x0: {
        uint32 op1Mask = 0b11000001011111;
        uint32 op2Mask = 0b100100;
        if (maskedBits(word, 9, 15, op1Mask) == 0 &&
            maskedBits(word, 0, 6, op2Mask) == 0)
          return True;

      }
      case 0x1:
      case 0x2:
      case 0x3:;
    }
  }
  return False;
}

static logical isSVE(uint32 word) {
  return !isReserved(word) && (op1(word) == 0x01);
}

static retCode outSVEInstruction(ioPo out, assemCtxPo ctx, uint32 word) {
  return outMsg(out, "SVE instruction");
}

retCode disassemble(ioPo out, assemCtxPo ctx, uint32 word) {
  if (isReserved(word))
    return outMsg(out, "reserved");
  else if (isSME(word))
    return outSVEInstruction(out, ctx, word);
  else if (isSVE(word))
    return outSVEInstruction(out, ctx, word);
  else
    return outMsg(out, "unknown");
}
