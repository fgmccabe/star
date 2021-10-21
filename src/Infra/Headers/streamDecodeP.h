//
// Created by Francis McCabe on 2019-03-17.
//

#ifndef STAR_STREAMDECODEP_H
#define STAR_STREAMDECODEP_H

#include "streamDecode.h"

#ifdef TRACEDECODE
extern tracingLevel traceDecode;
#endif

extern retCode decInt(ioPo in, integer *ii);
extern retCode decFlt(ioPo in, double *dx);
extern retCode decodeInteger(ioPo in, integer *ix);
extern retCode decodeText(ioPo in, strBufferPo buffer);
extern retCode decodeString(ioPo in, char *buffer, integer buffLen);
extern retCode decodeName(ioPo in, char *buffer, integer buffLen, integer *length);

typedef struct {
  char field[MAX_SYMB_LEN];
  integer arity;
  integer offset;
} FieldRec;

typedef retCode (*intProc)(integer ix, void *cl);
typedef retCode (*fltProc)(double dx, void *cl);
typedef retCode (*nameProc)(char *sx, integer ar, void *cl);
typedef retCode (*recLblProc)(char *sx, integer ar, FieldRec fields[], void *cl);
typedef retCode (*charProc)(codePoint cp, void *cl);
typedef retCode (*stringProc)(char *sx, integer len, void *cl);
typedef retCode (*consProc)(integer len, void *cl);
typedef retCode (*flagProc)(void *cl);

typedef struct {
  flagProc startDecoding;
  flagProc endDecoding;
  flagProc decVoid;
  intProc decInt;
  fltProc decFlt;
  nameProc decLbl;
  recLblProc decRecLbl;
  charProc decChar;
  stringProc decString;
  consProc decCons;
  flagProc endCons;
  consProc decLst;
  flagProc endLst;
} DecodeCallBacks, *decodeCallBackPo;

retCode streamDecode(ioPo in, decodeCallBackPo cb, void *cl, char *errorMsg, integer msgLen);

#endif //STAR_STREAMDECODEP_H
