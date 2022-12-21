//
// Created by Francis McCabe on 12/2/21.
//

#ifndef STAR_MULTIP_H
#define STAR_MULTIP_H

#include "multi.h"

typedef struct multi_record_ {
  integer size;
  uint32 *data;
} MultiRecord;

extern logical traceMulti;

#define SIGN_MASK 0x80000000
#define ONES_MASK 0xffffffff

logical longEqual(const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize);
comparison longCompare(const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize);

integer longAdd(uint32 *tgt, integer tSize, const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize);
integer longSubtract(uint32 *tgt, integer tCount, uint32 *a, integer aCount, uint32 *b, integer bCount);
integer longMultiply(uint32 *tgt, integer tSize, uint32 *a, integer aCount, uint32 *b, integer bCount);
retCode longDivide(uint32 *q, integer *qC, uint32 *r, integer *rC, uint32 *n, integer nC, uint32 *d, integer dC);
integer longGCD(uint32 *tgt, uint32 *a, integer aC, uint32 *b, integer bC);
uinteger longHash(uint32 *data, integer count);
integer longBitAnd(uint32 *tgt, integer tCount, uint32 *a, integer aCount, uint32 *b, integer bCount);
integer longBitNot(uint32 *tgt, integer tCount, uint32 *a, integer aCount);
integer longBitOr(uint32 *tgt, integer tCount, uint32 *a, integer aCount, uint32 *b, integer bCount);

integer longFormat(uint32 *data, integer count, const char *format, integer formatLen, char *buffer, integer buffLen);
integer longFromText(const char *text, integer tLen, uint32 *data, integer count);
integer textFromlong(char *text, integer tLen, uint32 *data, integer count);
retCode showLong(ioPo out, uint32 *digits, long count);

#endif //STAR_MULTIP_H
