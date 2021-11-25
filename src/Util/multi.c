//
// Created by Francis McCabe on 11/23/21.
//

#include <stdlib.h>
#include <assert.h>
#include "multi.h"

#define SIGN_MASK 0x80
#define ONES_MASK 0xff

typedef struct multi_record_ {
  integer size;
  byte *data;
} MultiRecord;

byte *multiData(multiPo num) {
  return num->data;
}

integer multiSize(multiPo num) {
  return num->size;
}

logical multiNegative(multiPo num) {
  return (logical) (((int8) (num->data[num->size - 1])) < 0);
}

static integer trimMulti(const byte *data, integer count) {
  byte msb = data[count - 1];

  if (msb == 0xff || msb == 0) {
    while (count > 1 && data[count - 1] == msb) {
      count--;
    }
    if (count > 1 && (data[count - 1] & 0x80) != (msb & 0x80))
      count++;  // we have significant bits in the sign
  }

  return count;
}

multiPo allocMulti(byte *data, integer count) {
  count = trimMulti(data, count);
  multiPo m = (multiPo) malloc(sizeof(MultiRecord));
  byte *b = (byte *) malloc(count);
  m->size = count;
  m->data = b;
  byteMove(m->data, count, data, count);
  return m;
}

static integer multiAdd(byte *tgt, integer tSize, const byte *lhs, integer lSize, const byte *rhs, integer rSize) {
  byte carry = 0;
  integer ix = 0;
  while (ix < tSize && (ix < lSize || ix < rSize)) {
    uint32 partial = (ix < lSize ? lhs[ix] : 0) + (ix < rSize ? rhs[ix] : 0) + carry;
    carry = (partial >> 8) & 0x3;
    tgt[ix++] = partial & 0xff;
  }
  return ix;
}

static integer multiComplement(byte *tgt, integer tSize, const byte *src, integer sSize) {
  assert(tSize >= sSize);
  byte carry = 1;
  integer ix = 0;
  while (ix < sSize) {
    uint16 partial = (0xff - src[ix]) + carry;
    carry = partial >> 8;
    tgt[ix++] = partial & 0xff;
  }
  if (carry != 0 && ix < tSize) {
    tgt[ix++] = carry;
  }
  return ix;
}

comparison multiCompare(multiPo a, multiPo b) {
  byte tA[a->size + 2];
  byte tB[b->size + 2];
  integer bSize = multiComplement(tB, b->size + 2, b->data, b->size);
  integer aSize = multiAdd(tA, a->size + 2, tA, a->size, tB, bSize);

  if ((tA[aSize - 1] & 0x80) == 0x80)
    return smaller;
  else if (tA[aSize - 1] == 0 && aSize == 1)
    return same;
  else
    return bigger;
}

multiPo multiPlus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  byte sum[max];
  integer len = multiAdd(sum, max, lhs->data, lhs->size, rhs->data, rhs->size);
  return allocMulti(sum, len);
}

multiPo multiMinus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  byte sum[max];
  multiComplement(sum, max, rhs->data, rhs->size);
  integer len = multiAdd(sum, max, lhs->data, lhs->size, sum, rhs->size);
  return allocMulti(sum, len);
}

static integer mulBy10Add(byte *data, integer len, byte carry) {
  integer ix = 0;
  while (ix < len) {
    uint16 segment = data[ix] * 10 + carry;
    carry = segment / 256;
    data[ix++] = segment & 0xff;
  }
  if (carry > 0)
    data[ix++] = carry;
  return ix;
}

multiPo multiFromText(char *text, integer tlen) {
  byte data[tlen];
  for (integer ix = 0; ix < tlen; ix++)
    data[ix] = 0;
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

static integer divBy10(byte *data, integer count, byte *last) {
  integer ix = count;
  uint16 work = 0;

  while (ix > 0) {
    work = work << 8 | data[--ix];
    uint8 q = work / 10;
    work = work % 10;
    data[ix] = q;
  }

  *last = work;
  while (count > 0 && data[count - 1] == 0)
    count--;
  return count;
}

static void multiTxt(char *text, integer *pos, byte *data, integer count) {
  if (count == 1 && data[0] < 10) {
    text[(*pos)++] = data[0] + '0';
  } else {
    byte last;
    count = divBy10(data, count, &last);
    multiTxt(text, pos, data, count);
    text[(*pos)++] = last + '0';
  }
}

integer multiText(char *text, integer tLen, multiPo num) {
  integer count = num->size;
  byte temp[count];
  integer pos = 0;

  if (multiNegative(num)) {
    multiComplement(temp, count, num->data, count);
    text[pos++] = '-';
  } else {
    byteMove(temp, count, num->data, count);
    text[pos++] = '+';
  }
  multiTxt(text, &pos, temp, count);
  return pos;
}

retCode showMulti(ioPo out, void *data, long depth, long precision, logical alt);

multiPo multiMinus(multiPo lhs, multiPo rhs);
multiPo multiTimes(multiPo lhs, multiPo rhs);
integer multiDivide(multiPo lhs, multiPo rhs, multiPo res);
integer multiQuotient(multiPo lhs, multiPo rhs, multiPo res);
integer multiRemainder(multiPo lhs, multiPo rhs, multiPo res);



