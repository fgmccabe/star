//
// Created by Francis McCabe on 2/3/17.
//

#ifndef STAR_ENCODEDP_H
#define STAR_ENCODEDP_H

#include "encoding.h"
#include "decode.h"
#include "stringBuffer.h"
#include "streamDecodeP.h"

typedef struct _encoding_support_ {
  char *errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, bufferPo strBuffer);
retCode decodeRecLbl(ioPo in, encodePo S, termPo *tgt, bufferPo tmpBuffer, char *errorMsg, integer msgSize);

#endif //STAR_ENCODEDP_H
