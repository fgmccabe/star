//
// Created by Francis McCabe on 2/3/17.
//

#ifndef STAR_ENCODED_H
#define STAR_ENCODED_H

#include "engine.h"
#include "encoding.h"
#include "array.h"
#include "stringBuffer.h"
#include "streamDecodeP.h"

typedef struct encoding_support_ {
  char *errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, strBufferPo strBuffer);

retCode decodeTplCount(ioPo in, int32 *count, char *errorMsg, integer msgSize);

retCode decodeInstructions(ioPo in, int32 *insCount, insPo *code, char *errorMsg, long msgSize, termPo constantPool);
#endif //STAR_ENCODED_H
