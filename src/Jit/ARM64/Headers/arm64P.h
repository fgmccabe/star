//
// Created by Francis McCabe on 7/10/20.
//

#ifndef STAR_ARM64P_H
#define STAR_ARM64P_H

#include "jitP.h"
#include "arm64.h"
#include "ooio.h"
#include "array.h"

void initAssem();
void clearCodeCtxMaps(assemCtxPo ctx);

void encodeSve(assemCtxPo ctx);

#define PREL 0x0
#define ADDSUBIMM 0x2
#define ADDSUBTAG 0x3
#define LOGICALIMM 0x4
#define MOVEWIDE 0x5
#define BITFIELD 0x6
#define EXTRACE 0x7

void encodeDataImm(uint8 op0,assemCtxPo ctx);

void encodeBranch(assemCtxPo ctx);
void encodeDataReg(assemCtxPo ctx);
void encodeDataFP(assemCtxPo ctx);



#endif //STAR_ARM64P_H
