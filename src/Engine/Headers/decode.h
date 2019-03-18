#ifndef _ENCODE_H_
#define _ENCODE_H_
#include "engine.h"
#include "encoding.h"
#include "streamDecode.h"

retCode decodeTerm(ioPo in, heapPo H, termPo *tgt, char *errorMsg, long msgSize);

typedef retCode (*decodeFun) (ioPo in,heapPo heap, char *errorMsg, long msgSize, integer ix, void *cl);

hashPo decodePkg(ioPo in,heapPo heap);

#endif
