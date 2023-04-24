//
// Created by Francis McCabe on 11/12/21.
//

#include <stdlib.h>
#include "bcdP.h"
#include "assert.h"

#define DIGITS_PER_WORD 8

static integer bcdSum(integer len, uint32 reslt[], uint32 lhs[], uint32 rhs[]);
static integer bcdSubtract(integer len, uint32 reslt[], uint32 lhs[], uint32 rhs[]);
static integer bcdComplement(integer len, uint32 reslt[], const uint32 digits[]);

static integer bcdTrim(const uint32 *data, integer len) {
  if (len > 1) {
    uint32 msw = data[len - 1];
    if (msw == 0 || msw == 0x99999999) {
      while (len > 1 && data[len - 1] == msw)
        len--;
    }
  }
  return len;
}

static uint8 msd(bcdPo bcd) {
  assert(bcd->count > 0);
  return ((bcd->data[bcd->count - 1]) >> 28) & 0xf;
}

sign bcdSign(bcdPo num) {
  if (msd(num) >= 5)
    return negative;
  else
    return positive;
}

// Slow, but sure.
static integer bcdMove(uint32 *tgt, integer tgtLen, const uint32 *src, integer srcCount) {
  assert(tgtLen >= srcCount);

  for (integer ix = 0; ix < srcCount; ix++) {
    tgt[ix] = src[ix];
  }

  uint32 extend = (src[srcCount - 1] >= 0x5000000 ? 0x99999999 : 0x00000000);

  for (integer ix = srcCount; ix < tgtLen; ix++)
    tgt[ix] = extend;
  return tgtLen;
}

bcdPo allocBCD(integer count, uint32 *data) {
  count = bcdTrim(data, count);

  size_t size = sizeof(BcdNumberRecord) + (count * sizeof(uint32));

  bcdPo bcd = (bcdPo) malloc(size);
  bcd->count = count;
  bcdMove(bcd->data, count, data, count);
  return bcd;
}

integer textOfBCD(char *text, integer tlen, bcdPo num) {
  integer pos = 0;
  integer max = num->count;
  uint32 data[max];
  integer len;

  if (msd(num) > 5) {
    text[pos++] = '-';
    len = bcdComplement(num->count, data, num->data);
  } else {
    text[pos++] = '+';
    len = bcdMove(data, max, num->data, num->count);
  }

  logical leading = True;
  for (integer ix = len - 1; ix >= 0; ix--) {
    uint32 segment = data[ix];
    for (integer cx = DIGITS_PER_WORD - 1; cx >= 0; cx--) {
      uint8 nibble = (segment >> (4 * cx)) & 0xf;
      if (nibble == 0 && leading)
        continue;
      else {
        text[pos++] = "0123456789"[nibble];
        leading = False;
      }
    }
  }
  return pos;
}

retCode showBCD(ioPo out, void *data, long depth, long precision, logical alt) {
  bcdPo bcd = (bcdPo) data;

  char text[bcd->count + 2];
  integer tlen = textOfBCD(text, bcd->count + 2, bcd);
  return outText(out, text, tlen);
}

bcdPo bcdFromText(char *text, integer tlen) {
  integer px = 0;
  sign sign;
  uint32 data[tlen]; // more than enough

  if (text[px] == '-') {
    sign = negative;
    px++;
  } else if (text[px] == '+') {
    sign = positive;
    px++;
  } else {
    sign = positive;
  }

  integer txpos = tlen;
  integer cx = 0;
  uint32 segment = 0;
  int dx = 0;
  while (txpos > px) {
    codePoint ch = prevCodePoint(text, &txpos);
    if (isNdChar(ch)) {
      int digit = digitValue(ch);
      segment |= (digit & 0xf) << dx;
      dx += 4;
      if (dx == 32) {
        dx = 0;
        data[cx++] = segment;
        segment = 0;
      }
    }
  }

  data[cx++] = segment;
  if (segment >= 0x50000000)
    data[cx++] = 0;  // Add a zero to avoid confusing complementation

  if (sign == negative) {
    cx = bcdComplement(cx, data, data);
  }

  return allocBCD(cx, data);
}

integer bcdSum(integer len, uint32 reslt[], uint32 lhs[], uint32 rhs[]) {
  uint8 carry = 0;

  for (integer ix = 0; ix < len; ix++) {
    uint64 block = bcd_add(lhs[ix], rhs[ix]);
    uint32 c1 = (block >> 32) & 0xf;
    block = bcd_add(block, carry);
    carry = c1 | ((block >> 32) & 0xf); // Carry out could come from either addition
    reslt[ix] = block & 0xffffffff;
  }
  return len;
}

integer bcdComplement(integer len, uint32 reslt[], const uint32 digits[]) {
  for (integer ix = 0; ix < len; ix++) {
    reslt[ix] = 0x99999999 - digits[ix];
  }

  uint32 one[len];
  one[0] = 1;
  for (integer ix = 1; ix < len; ix++)
    one[ix] = 0;

  return bcdSum(len, reslt, reslt, one);
}

static integer bcdSubtract(integer len, uint32 reslt[], uint32 lhs[], uint32 rhs[]) {
  bcdComplement(len, reslt, rhs);
  return bcdSum(len, reslt, lhs, reslt);
}

bcdPo bcdPlus(bcdPo lhs, bcdPo rhs) {
  integer rsize = maximum(lhs->count, rhs->count) + 1;
  uint32 ldata[rsize];
  uint32 rdata[rsize];
  uint32 xdata[rsize];

  bcdMove(ldata, rsize, lhs->data, lhs->count);
  bcdMove(rdata, rsize, rhs->data, rhs->count);

  integer len = bcdSum(rsize, xdata, ldata, rdata);

  return allocBCD(len, xdata);
}

bcdPo bcdMinus(bcdPo lhs, bcdPo rhs) {
  integer rsize = maximum(lhs->count, rhs->count) + 1;
  uint32 ldata[rsize];
  uint32 rdata[rsize];
  uint32 xdata[rsize];

  bcdMove(ldata, rsize, lhs->data, lhs->count);
  bcdMove(rdata, rsize, rhs->data, rhs->count);

  integer len = bcdSubtract(rsize, xdata, ldata, rdata);
  return allocBCD(len, xdata);
}

uint64 bcd_add(uint32 a, uint32 b) {
  uint64 t1 = a + 0x066666666;
  uint64 t2 = t1 + b;
  uint64 t3 = t1 ^ b;
  uint64 t4 = t2 ^ t3;
  uint64 t5 = ~t4 & 0x111111110;
  uint64 t6 = (t5 >> 2) | (t5 >> 3);
  return t2 - t6;
}

static uint8 nibble(uint64 a, uint8 cx) {
  return (a >> (cx << 2)) & 0xf;
}

uint64 mask[] = {
  0x000000000000000f,
  0x00000000000000f0,
  0x0000000000000f00,
  0x000000000000f000,
  0x00000000000f0000,
  0x0000000000f00000,
  0x000000000f000000,
  0x00000000f0000000,
  0x0000000f00000000,
  0x000000f000000000,
  0x00000f0000000000,
  0x0000f00000000000,
  0x000f000000000000,
  0x00f0000000000000,
  0x0f00000000000000,
  0xf000000000000000
};

static uint64 setNibble(uint64 a, uint8 cx, uint8 nibble) {
  return (((uint64) (nibble & 0xf)) << (cx << 2)) | (a & ~mask[cx]);
}

static void zeroFill(uint32 *a, integer count) {
  for (integer ix = 0; ix < count; ix++)
    a[ix] = 0;
}

static uint8 nbl(uint32 *src, integer ix) {
  return nibble(src[ix / DIGITS_PER_WORD], ix % DIGITS_PER_WORD);
}

static void setNbl(uint32 *tgt, integer i, uint8 d) {
  integer ix = i / DIGITS_PER_WORD;
  tgt[ix] = setNibble(tgt[ix], i % DIGITS_PER_WORD, d);
}

// Assumes both numbers are positive
static long longMultiply(uint32 *tgt, uint32 *a, integer aCount, uint32 *b, integer bCount) {
  zeroFill(tgt, aCount + bCount);
  for (integer i = 0; i < aCount * DIGITS_PER_WORD; i++) {
    uint8 nA = nbl(a, i);
    if (nA != 0) {
      uint8 carry = 0;

      for (integer j = 0; j < bCount * DIGITS_PER_WORD; j++) {
        uint8 d = carry + nA * nbl(b, j) + nbl(tgt, i + j);
        carry = d / 10;
        setNbl(tgt, i + j, d % 10);
      }
      setNbl(tgt, i + bCount * DIGITS_PER_WORD, carry);
    }
  }
  return aCount + bCount;
}

bcdPo bcdTimes(bcdPo lhs, bcdPo rhs) {
  integer rsize = lhs->count + rhs->count;
  uint32 prod[rsize];

  switch (bcdSign(lhs)) {
    case positive: {
      switch (bcdSign(rhs)) {
        case positive: {
          longMultiply(prod, lhs->data, lhs->count, rhs->data, rhs->count);
          return allocBCD(rsize, prod);
        }
        case negative: {
          uint32 rrhs[rhs->count];
          bcdComplement(rhs->count, rrhs, rhs->data);
          longMultiply(prod, lhs->data, lhs->count, rrhs, rhs->count);
          bcdComplement(rsize, prod, prod);
          return allocBCD(rsize, prod);
        }
      }
    }
    case negative: {
      uint32 llhs[lhs->count];
      bcdComplement(lhs->count, llhs, lhs->data);
      switch (bcdSign(rhs)) {
        case positive: {
          longMultiply(prod, llhs, lhs->count, rhs->data, rhs->count);
          bcdComplement(rsize, prod, prod);
          return allocBCD(rsize, prod);
        }
        case negative: {
          uint32 rrhs[rhs->count];
          bcdComplement(rhs->count, rrhs, rhs->data);
          longMultiply(prod, llhs, lhs->count, rrhs, rhs->count);
          return allocBCD(rsize, prod);
        }
      }
    }
  }
}

// Compare unsigned number strings
static comparison compareDigits(const uint32 *lhs, integer lCount, const uint32 *rhs, integer rCount) {
  if (lCount > rCount)
    return bigger;
  else if (lCount < rCount)
    return smaller;
  else {
    for (integer ix = lCount - 1; ix >= 0; ix--) {
      if (lhs[ix] > rhs[ix])
        return bigger;
      else if (lhs[ix] < rhs[ix])
        return smaller;
    }
    return same;
  }
}

comparison bcdCompare(bcdPo a, bcdPo b) {
  return compareDigits(a->data, a->count, b->data, b->count);
}

logical sameBCD(bcdPo lhs, bcdPo rhs) {
  return (logical) (bcdCompare(lhs, rhs) == same);
}

// compute a/b to give q & r
static long longDivide(uint32 *q, uint32 *r, integer qCount, uint32 *a, integer aCount, uint32 *b, integer bCount) {
  zeroFill(q, qCount);
  zeroFill(r, qCount);
  uint32 w[aCount];
  bcdMove(w, aCount, a, aCount);

  for (integer i = aCount * DIGITS_PER_WORD - 1; i >= 0; i--) {

  }
  return aCount + bCount;
}
