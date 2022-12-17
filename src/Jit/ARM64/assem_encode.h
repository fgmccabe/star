//
// Created by Francis McCabe on 12/15/22.
//

#ifndef STAR_ASSEM_ENCODE_H
#define STAR_ASSEM_ENCODE_H

#include "arm64P.h"

void encodePCRel(uint1 op, codeLblPo lbl, armReg Rd, assemCtxPo ctx);
void encodeReg2Src(uint1 wide, uint1 o, uint1 S, uint8 op1, armReg Rm, uint8 op2, armReg Rn, armReg Rd, assemCtxPo ctx);
void encode4Reg(uint1 w, uint8 opc, uint8 op, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeSz4Reg(uint8 sz, uint8 op, uint8 L, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx);
void encode2SrcExt(uint1 wide, uint1 o, uint1 S, uint8 op, armReg Rm, armExtent ex, uint8 imm, armReg Rn, armReg Rd,
                   assemCtxPo ctx);
void encodeDPRegImm(uint1 wide, uint8 opc, uint8 op, uint1 sh, uint16 imm, armReg Rn, armReg Rd, assemCtxPo ctx);
void encode3RegExtended(uint1 w, uint1 opt, uint1 S, uint8 op, armReg Rm, uint8 option, uint8 imm, armReg Rn, armReg Rd,
                        assemCtxPo ctx);
void encodeAddSubImm(uint1 w, uint1 op, uint1 S, uint8 code, uint1 sh, int16 imm, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeLogImm(uint1 w, uint8 opc, uint64 val, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeMovWide(uint1 w, uint8 opc, uint8 hw, int16 imm, armReg Rd, assemCtxPo ctx);
void encodeImmRegReg(uint1 w, uint8 opc, uint1 N, uint8 immr, uint8 imms, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeExtrct(uint1 w, uint8 opc, uint1 N, uint1 o0, armReg Rm, uint8 imms, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeCondBrnch(uint8 op, uint1 o1, codeLblPo lbl,armCond cond, assemCtxPo ctx);
void encodeBranch(uint8 opc, uint8 op2, uint8 op3, armReg Rn, uint8 op4, assemCtxPo ctx);
void encodeBranchImm(uint1 op, codeLblPo lbl, assemCtxPo ctx);
void encodeCmpBr(uint1 b5, uint1 op, codeLblPo lbl, armReg Rt, assemCtxPo ctx);
void encodeTstBr(uint1 w, uint1 op, uint8 b40, codeLblPo lbl, armReg Rt, assemCtxPo ctx);
void encodeCasPr(uint8 w, uint1 L, armReg Rs, uint o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeCas(uint8 w, uint1 L, armReg Rs, uint o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeExLdStPr(uint1 w, uint1 L, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeExLdStRg(uint8 w, uint1 L, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStOrd(uint8 w, uint1 L, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStUnscaled(uint8 w, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdPcLit(uint8 opc, uint1 V, codeLblPo lbl, armReg Rt, assemCtxPo ctx);
void encodeLdStNoAl(uint8 opc, uint1 V, uint1 L, uint8 imm7, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStRegUnscaled(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStRegPost(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStRegPre(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdRegLit(uint8 sz, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeLdStRegOff(uint8 opc, uint1 V, uint1 L, int8 imm7, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeIxReg(uint8 w, uint1 V, int8 opc, int8 imm, armReg Rn, armReg Rt, ixMode ix, assemCtxPo ctx);
void encodeLdStUnPriv(uint8 sz, uint1 V, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx);
void
encodeLdSt3Reg(uint8 sz, uint1 V, uint8 opc, armReg Rm, uint8 option, uint1 S, armReg Rn, armReg Rt, assemCtxPo ctx);
void encodeSz2OpcImm3Reg(uint8 sz, uint8 op, uint8 opc, uint1 N, armReg Rm, armExtent ex, uint1 S, uint8 S1, armReg Rn,
                         armReg Rt, assemCtxPo ctx);
void
encode3Reg7Imm(uint1 w, uint8 opc, uint1 S, uint8 op, uint8 sh, uint1 L, armReg Rm, uint8 imm, armReg Rn, armReg Rd,
               assemCtxPo ctx);
void encodeImm3Reg(uint8 sz, uint8 opc, uint8 op, armReg Rm, uint8 imm, armReg Rn, armReg Rd, assemCtxPo ctx);
void encodeDPReg2Imm(uint1 wide, uint8 opc, uint8 op, uint1 N, uint8 imm1, uint8 imm2, armReg Rn, armReg Rd,
                     assemCtxPo ctx);
void encodeShift3Reg(uint1 wide, uint1 o, uint1 S, uint8 op, armShift sh, uint1 N, armReg Rm, int8 imm, armReg Rn,
                     armReg Rd, assemCtxPo ctx);
void encode2Imm1Src(uint1 w, uint8 op, uint8 immr, uint8 imms, armReg R1, armReg RD, assemCtxPo ctx);
void encodeCondTgt(uint8 op, uint32 imm, armCond cond, assemCtxPo ctx);
void encodeCnd3Reg(uint1 w, uint1 o, uint1 S, uint8 op, armReg Rm, armCond cond, uint1 o2, armReg Rn, armReg Rd,
                   assemCtxPo ctx);
void encodeRelTgt(uint8 op, uint32 imm, assemCtxPo ctx);
void encodeRelReg(uint1 wide, uint8 op, uint32 imm, armReg Rt, assemCtxPo ctx);
void encodeCmpImm(uint1 w, uint8 o0, uint8 op, uint8 imm, armCond cond, armReg Rn, uint8 nzcv, assemCtxPo ctx);

#endif //STAR_ASSEM_ENCODE_H
