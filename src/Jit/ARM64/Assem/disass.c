//
// Created by Francis McCabe on 5/18/25.
//

#include "disassP.h"

static uint32 bits(uint32 word, uint8 from, uint8 count) {
  return (word >> from) & ((1u << count) - 1u);
}

static uint32 maskedBits(uint32 word, uint8 from, uint8 count, uint32 mask) {
  return bits(word, from, count) & mask;
}

static uint32 masked(uint32 word, uint32 mask) {
  return (word & mask);
}

static uint8 op0(uint32 word) {
  return (uint8) bits(word, 29, 2);
}

static uint8 op1(uint32 word) {
  return (uint8) bits(word, 24, 8);
}

static retCode isUnallocated(ioPo out, uint32 word) {
  return outMsg(out, "Unallocated #0x%x", word);
}

static logical isReserved(uint32 word) {
  return bits(word, 31, 1) == 0 && bits(word, 25, 4) == 0b0000;
}

static retCode decodeReserved(ioPo out, uint32 word, assemCtxPo ctx) {
  if (op0(word) == 0) {
    if (bits(word, 16, 9) == 0b0) {
      return outMsg(out, "UDF #0x%x", bits(word, 0, 16));
    }
    if (bits(word, 16, 9) == 0b1 || bits(word, 17, 8) == 0b1 ||
        bits(word, 18, 7) == 0b1 || bits(word, 19, 6) == 0b1 ||
        bits(word, 20, 5) == 0b1 || bits(word, 21, 4) == 0b1 ||
        bits(word, 22, 3) == 0b1 || bits(word, 23, 2) == 0b1 ||
        bits(word, 24, 1) == 0b1) {
      return isUnallocated(out, word);
    }
  }
  if (op0(word) == 1 || op0(word) == 2 || op0(word) == 3)
    return isUnallocated(out, word);
  return outMsg(out, "Bad instruction 0x%x", word);
}

static logical isSME(uint32 word) {
  return bits(word, 31, 1) == 0b1 && bits(word, 25, 4) == 0;
}

static retCode decodeSMEOuterProduct(ioPo out, uint32 word, assemCtxPo ctx) {
  return Error;
}

static retCode decodeSME2OuterProduct(ioPo out, uint32 word, assemCtxPo ctx) {
  return Error;
}

static retCode decodeSME(ioPo out, uint32 word, assemCtxPo ctx) {
  uint32 op0 = bits(word, 29, 2);
  uint32 op1 = bits(word, 10, 15);
  uint32 op2 = bits(word, 1, 4);

  if (bits(word, 31, 1) == 1) {
    if (masked(op0, 0b10) == 0) {
      if (bits(op1, 12, 2) == 0b10 && bits(op2, 1, 1) == 1)
        return isUnallocated(out, word);
      else if (bits(op1, 12, 2) == 0b11) {
        if (bits(op2, 2, 1) == 0)
          return decodeSMEOuterProduct(out, word, ctx);
        else
          return isUnallocated(out, word);
      }
      return outMsg(out, "Bad instruction 0x%x", word);
    }
    if (op0 == 0) {
      if (bits(op1, 14, 1) == 0)
        return isUnallocated(out, word);
      if (bits(op1, 13, 2) == 0b10) {
        if (bits(op2, 1, 2) == 0)
          return decodeSMEOuterProduct(out, word, ctx);
        if (bits(op2, 1, 2) == 0b10)
          return decodeSME2OuterProduct(out, word, ctx);
      }
    }
  }
  return outMsg(out, "bad instruction: 0x%x", word);
}

static logical isSVE(uint32 word) {
  return !isReserved(word) && (op1(word) == 0x01);
}

static retCode decodeSVE(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

static logical isDPImm(uint32 word) {
  if (bits(word, 29, 2) == 0b11) {
    switch (bits(word, 22, 4)) {
      case 0b1110:
      case 0b1111:
      case 0b0:
      case 0b1:
      case 0b10:
      case 0b11:
      case 0b0100:
      case 0b0101:
      case 0b0110:
      case 0b0111:
      case 0b1000:
      case 0b1001:
      case 0b1010:
      case 0b1011:
      case 0b1100:
      case 0b1101:
        return True;
      default:
        return False;
    }
  } else if (bits(word, 23, 3) == 0x111b) {
    return True;
  } else
    return False;
}

static retCode decodeDPImm(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

static logical isBranchOp(uint32 word) {
  uint32 op0 = bits(word, 31, 1);
  uint32 op1 = bits(word, 25, 4);

  return (op0 == 1 && masked(op1, 0b110) == 0b1010);
}

static retCode decodeBranch(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

static logical isDPRegOp(uint32 word) {
  uint32 op0 = bits(word, 31, 1);
  uint32 op1 = bits(word, 25, 4);

  return (op0 == 1 && masked(op1, 0b111) == 0b101);
}

static retCode decodeDPReg(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

static logical isDPSclrOp(uint32 word) {
  uint32 op0 = bits(word, 31, 1);
  uint32 op1 = bits(word, 25, 4);

  return (op0 == 1 && masked(op1, 0b111) == 0b111);
}

static retCode decodeDPSclr(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

static logical isLdStOp(uint32 word) {
  uint32 op0 = bits(word, 31, 1);
  uint32 op1 = bits(word, 25, 4);

  return (op0 == 1 && masked(op1, 0b101) == 0b100);
}

static retCode decodeLdSt(ioPo out, uint32 word, assemCtxPo ctx) {
  return outMsg(out, "SVE instruction");
}

retCode disassIns(ioPo out, uint32 word, assemCtxPo ctx) {
  if (isReserved(word))
    return decodeReserved(out, word, ctx);
  else if (isSME(word))
    return decodeSME(out, word, ctx);
  else if (isSVE(word))
    return decodeSVE(out, word, ctx);
  else if (isDPImm(word))
    return decodeDPImm(out, word, ctx);
  else if (isBranchOp(word))
    return decodeBranch(out, word, ctx);
  else if (isDPRegOp(word))
    return decodeDPReg(out, word, ctx);
  else if (isDPSclrOp(word))
    return decodeDPSclr(out, word, ctx);
  else if (isDPSclrOp(word))
    return decodeLdSt(out, word, ctx);
  else
    return outMsg(out, "bad instruction: 0x%x", word);
}
