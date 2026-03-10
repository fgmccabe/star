//
// Created by Francis McCabe on 2/3/17.
//

#ifndef STAR_ENCODED_H
#define STAR_ENCODED_H

#include "engine.h"
#include "encoding.h"
#include "stringBuffer.h"
#include "streamDecodeP.h"

typedef struct encoding_support_ {
  char *errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

typedef enum {
  hardDef,
  softDef
} DefinitionMode;

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, strBufferPo strBuffer);

retCode decodeTplCount(ioPo in, int32 *count, char *errorMsg, integer msgSize);

retCode loadCode(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize);
#endif //STAR_ENCODED_H
