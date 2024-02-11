//
// Created by Francis McCabe on 1/15/18.
//

#ifndef STAR_ERRORCODES_H
#define STAR_ERRORCODES_H

#include "term.h"

extern termPo eOk;
extern termPo eINTRUPT;
extern termPo eINVAL;
extern termPo eRANGE;
extern termPo eNOFILE;
extern termPo eNOTDIR;
extern termPo eNOTFND;
extern termPo eNOPERM;
extern termPo eFAIL;
extern termPo eIOERROR;
extern termPo eCONNECT;
extern termPo eDEAD;
extern termPo divZero;
extern termPo noValue;
extern termPo hasValue;

termPo ioErrorCode(retCode ret);

#endif //STAR_ERRORCODES_H
