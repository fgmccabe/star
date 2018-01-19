#ifndef _ENCODE_H_
#define _ENCODE_H_
#include "engine.h"
#include "encoding.h"

retCode decodeTerm(ioPo in, heapPo H, termPo *tgt, char *errorMsg, long msgSize);
retCode skipEncoded(ioPo in, char *errorMsg, long msgLen);
retCode copyEncoded(ioPo in, ioPo out, char *errorMsg, long msgLen);

typedef retCode (*decodeFun) (ioPo in,heapPo heap, char *errorMsg, long msgSize, integer ix, void *cl);

retCode processTpl(ioPo in,heapPo heap, char *errorMsg, long msgSize, decodeFun dec,void *cl);

hashPo decodePkg(ioPo in,heapPo heap);

#endif
