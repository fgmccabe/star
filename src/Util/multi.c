//
// Created by Francis McCabe on 11/23/21.
//

#include <stdlib.h>
#include <assert.h>
#include "multi.h"

#define SIGN_MASK 0x80000000
#define ONES_MASK 0xffffffff
#define WIDTH 32

typedef struct multi_record_ {
  integer size;
  uint32 *data;
} MultiRecord;

uint32 *multiData(multiPo num) {
  return num->data;
}

integer multiSize(multiPo num) {
  return num->size;
}

logical multiNegative(multiPo num) {
  return (logical) ((num->data[num->size - 1] & SIGN_MASK) == SIGN_MASK);
}

static integer trimMulti(const uint32 *data, integer count) {
  uint32 msb = data[count - 1];

  if (msb == ONES_MASK || msb == 0) {
    while (count > 1 && data[count - 1] == msb) {
      count--;
    }
    if (count > 0 && (data[count - 1] & SIGN_MASK) != (msb & SIGN_MASK))
      count++;  // we have significant bits in the sign
  }

  return count;
}

static void dataMove(uint32 *tgt, const uint32 *src, integer count) {
  for (integer ix = 0; ix < count; ix++)
    tgt[ix] = src[ix];
}

static void zeroFill(uint32 *tgt, integer count) {
  for (integer ix = 0; ix < count; ix++)
    tgt[ix] = 0;
}

multiPo allocMulti(uint32 *data, integer count) {
  count = trimMulti(data, count);
  multiPo m = (multiPo) malloc(sizeof(MultiRecord));
  uint32 *b = (uint32 *) malloc(count * sizeof(uint32));
  m->size = count;
  m->data = b;
  dataMove(m->data, data, count);
  return m;
}

static integer
multiAdd(uint32 *tgt, integer tSize, const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize) {
  uint64 carry = 0;
  integer ix = 0;
  while (ix < tSize && (ix < lSize || ix < rSize)) {
    uint64 partial = (uint64)(ix < lSize ? lhs[ix] : 0) + (uint64)(ix < rSize ? rhs[ix] : 0) + carry;
    carry = (partial >> WIDTH) & (ONES_MASK >> 1);
    tgt[ix++] = partial & ONES_MASK;
  }
  return ix;
}

static integer multiComplement(uint32 *tgt, integer tSize, const uint32 *src, integer sSize) {
  assert(tSize >= sSize);
  uint32 carry = 1;
  integer ix = 0;
  while (ix < sSize) {
    uint64 partial = ~src[ix] + carry;
    carry = partial >> WIDTH;
    tgt[ix++] = partial & ONES_MASK;
  }
  if (carry != 0 && ix < tSize) {
    tgt[ix++] = carry;
  }
  return ix;
}

comparison multiCompare(multiPo a, multiPo b) {
  uint32 tA[a->size + 2];
  uint32 tB[b->size + 2];
  integer bSize = multiComplement(tB, b->size + 2, b->data, b->size);
  integer aSize = trimMulti(tA,multiAdd(tA, a->size + 2, a->data, a->size, tB, bSize));

  if ((tA[aSize - 1] & SIGN_MASK) == SIGN_MASK)
    return smaller;
  else if (tA[aSize - 1] == 0 && aSize == 1)
    return same;
  else
    return bigger;
}

multiPo multiPlus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  uint32 sum[max];
  integer len = multiAdd(sum, max, lhs->data, lhs->size, rhs->data, rhs->size);
  return allocMulti(sum, len);
}

multiPo multiMinus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  uint32 sum[max];
  multiComplement(sum, max, rhs->data, rhs->size);
  integer len = multiAdd(sum, max, lhs->data, lhs->size, sum, rhs->size);
  return allocMulti(sum, len);
}

static integer mulBy10Add(uint32 *data, integer len, uint32 carry) {
  integer ix = 0;
  while (ix < len) {
    uint64 segment = ((uint64)data[ix]) * 10 + carry;
    carry = segment >> WIDTH;
    data[ix++] = segment & ONES_MASK;
  }
  if (carry > 0)
    data[ix++] = carry;
  return ix;
}

multiPo multiFromText(char *text, integer tlen) {
  uint32 data[tlen];
  zeroFill(data, tlen);
  logical positive = True;
  integer px = 0;
  if (text[px] == '-') {
    positive = False;
    px++;
  } else if (text[px] == '+') {
    positive = True;
    px++;
  }

  integer dLen = 0;
  while (px < tlen) {
    codePoint ch = nextCodePoint(text, &px, tlen);
    if (isNdChar(ch)) {
      byte digit = digitValue(ch);
      dLen = mulBy10Add(data, dLen, digit);
    }
  }
  // Allow for case where number might look negative
  dLen++;

  if (positive) {
    return allocMulti(data, dLen);
  } else {
    dLen = multiComplement(data, dLen, data, dLen);
    return allocMulti(data, dLen);
  }
}

static integer divBy10(uint32 *data, integer count, uint32 *last) {
  integer ix = count;
  uint64 work = 0;

  while (ix > 0) {
    work = work << WIDTH | data[ix-1];
    uint64 q = work / 10;
    work = work % 10;
    data[--ix] = q;
  }
  while(count>0 && data[count-1]==0)
    count--;

  *last = work;
  return count;
}

static void multiTxt(char *text, integer *pos, uint32 *data, integer count) {
  if (count == 1 && data[0] < 10) {
    text[(*pos)++] = ((char) (data[0] + '0'));
  } else {
    uint32 last;
    count = divBy10(data, count, &last);
    multiTxt(text, pos, data, count);
    text[(*pos)++] = ((char) (last + '0'));
  }
}

integer multiText(char *text, integer tLen, multiPo num) {
  integer count = num->size;
  uint32 temp[count];
  integer pos = 0;

  if (multiNegative(num)) {
    multiComplement(temp, count, num->data, count);
    text[pos++] = '-';
  } else {
    dataMove(temp, num->data, count);
    text[pos++] = '+';
  }
  multiTxt(text, &pos, temp, count);
  return pos;
}

retCode showMulti(ioPo out, void *data, long depth, long precision, logical alt);

multiPo multiTimes(multiPo lhs, multiPo rhs);
integer multiDivide(multiPo lhs, multiPo rhs, multiPo res);
integer multiQuotient(multiPo lhs, multiPo rhs, multiPo res);
integer multiRemainder(multiPo lhs, multiPo rhs, multiPo res);



