//
// Created by Francis McCabe on 2/3/17.
//

#ifndef STAR_ENCODEDP_H
#define STAR_ENCODEDP_H

#include "encoding.h"
#include "decode.h"
#include "array.h"
#include "stringBuffer.h"
#include "streamDecodeP.h"

typedef struct _encoding_support_ {
  char *errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, strBufferPo strBuffer);

retCode decodeTplCount(ioPo in, int32 *count, char *errorMsg, integer msgSize);

retCode decodeInstructions(ioPo in, int32 *insCount, insPo *code, char *errorMsg, long msgSize);
#endif //STAR_ENCODEDP_H
