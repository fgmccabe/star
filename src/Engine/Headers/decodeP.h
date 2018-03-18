//
// Created by Francis McCabe on 2/3/17.
//

#ifndef LANDO_ENCODEDP_H
#define LANDO_ENCODEDP_H

#include "encoding.h"
#include "decode.h"
#include "stringBuffer.h"

typedef struct _encoding_support_ {
  char *errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

extern retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, bufferPo strBuffer);

extern retCode decInt(ioPo in, integer *ii);
extern retCode decodeInteger(ioPo in,integer *ix);
extern retCode decodeText(ioPo in, bufferPo buffer);
extern retCode decodeName(ioPo in, bufferPo buffer);
extern retCode decodeString(ioPo in,char *buffer, integer buffLen);

typedef retCode (*intProc)(integer ix, void *cl);
typedef retCode (*fltProc)(double dx, void *cl);
typedef retCode (*nameProc)(char *sx, integer ar, void *cl);
typedef retCode (*stringProc)(char *sx, integer len, void *cl);
typedef retCode (*consProc)(integer len, void *cl);
typedef retCode (*flagProc)(void *cl);

typedef struct {
  flagProc startDecoding;
  flagProc endDecoding;
  intProc decInt;
  fltProc decFlt;
  nameProc decLbl;
  stringProc decString;
  consProc decCons;
} DecodeCallBacks, *decodeCallBackPo;

retCode streamDecode(ioPo in, decodeCallBackPo cb, void *cl);

#endif //LANDO_ENCODEDP_H
