//
// Created by Francis McCabe on 11/23/21.
//

#include <stdlib.h>
#include <assert.h>
#include "multiP.h"
#include "formatted.h"

#define WIDTH 32

static uint32 longOne[] = {1};
static uint32 longZero[] = {0};

logical traceMulti = False;

uint32 *multiData(multiPo num) {
  return num->data;
}

integer multiSize(multiPo num) {
  return num->size;
}

sign multiSign(multiPo num) {
  if ((num->data[num->size - 1] & SIGN_MASK) == SIGN_MASK)
    return negative;
  else
    return positive;
}

static logical longNegative(const uint32 *data, integer count) {
  return (logical) ((data[count - 1] & SIGN_MASK) == SIGN_MASK);
}

logical multiNegative(multiPo num) {
  return longNegative(num->data, num->size);
}

static integer longTrim(const uint32 *data, integer count) {
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
  count = longTrim(data, count);
  multiPo m = (multiPo) malloc(sizeof(MultiRecord));
  uint32 *b = (uint32 *) malloc(count * sizeof(uint32));
  m->size = count;
  m->data = b;
  dataMove(m->data, data, count);
  return m;
}

void freeMulti(multiPo m) {
  free(m->data);
  m->data = Null;
  free(m);
}

integer longAdd(uint32 *tgt, integer tSize, const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize) {
  uint64 carry = 0;
  integer ix = 0;
  uint32 extendLhs = (longNegative(lhs, lSize) ? ONES_MASK : 0);
  uint32 extendRhs = (longNegative(rhs, rSize) ? ONES_MASK : 0);
  logical sameSign = longNegative(lhs, lSize) == longNegative(rhs, rSize);
  while (ix < tSize && (ix < lSize || ix < rSize)) {
    uint64 partial = (uint64) (ix < lSize ? lhs[ix] : extendLhs) + (uint64) (ix < rSize ? rhs[ix] : extendRhs) + carry;
    carry = (partial >> WIDTH) & (ONES_MASK >> 1);
    tgt[ix++] = partial & ONES_MASK;
  }
  if (carry != 0 && sameSign && ix < tSize)
    tgt[ix++] = carry & ONES_MASK;
  return longTrim(tgt, ix);
}

static integer longComplement(uint32 *tgt, const uint32 *src, integer sSize) {
  uint32 carry = 1;
  integer ix = 0;
  while (ix < sSize) {
    uint64 partial = (uint64) ~src[ix] + carry;
    carry = partial >> WIDTH;
    tgt[ix++] = partial & ONES_MASK;
  }
  if (carry != 0) {
    tgt[ix++] = carry;
  }
  return longTrim(tgt, ix);
}

comparison longCompare(const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize) {
  uint32 tA[lSize + 2];
  uint32 tB[rSize + 2];
  integer bSize = longComplement(tB, rhs, rSize);
  integer aSize = longTrim(tA, longAdd(tA, lSize + 2, lhs, lSize, tB, bSize));

  if ((tA[aSize - 1] & SIGN_MASK) == SIGN_MASK)
    return smaller;
  else if (tA[aSize - 1] == 0 && aSize == 1)
    return same;
  else
    return bigger;
}

comparison multiCompare(multiPo a, multiPo b) {
  uint32 tA[a->size + 2];
  uint32 tB[b->size + 2];
  integer bSize = longComplement(tB, b->data, b->size);
  integer aSize = longTrim(tA, longAdd(tA, a->size + 2, a->data, a->size, tB, bSize));

  if ((tA[aSize - 1] & SIGN_MASK) == SIGN_MASK)
    return smaller;
  else if (tA[aSize - 1] == 0 && aSize == 1)
    return same;
  else
    return bigger;
}

logical longEqual(const uint32 *lhs, integer lSize, const uint32 *rhs, integer rSize) {
  if (lSize == rSize) {
    for (integer ix = 0; ix < lSize; ix++) {
      if (lhs[ix] != rhs[ix])
        return False;
    }
    return True;
  }
  return False;
}

static logical longIsZero(uint32 *a, integer aC) {
  return longEqual(a, aC, longZero, NumberOf(longZero));
}

logical sameMulti(multiPo a, multiPo b) {
  if (a->size != b->size)
    return False;
  else {
    for (integer ix = 0; ix < a->size; ix++)
      if (a->data[ix] != b->data[ix])
        return False;
    return True;
  }
}

multiPo multiPlus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  uint32 sum[max];
  integer len = longAdd(sum, max, lhs->data, lhs->size, rhs->data, rhs->size);
  return allocMulti(sum, len);
}

integer longSubtract(uint32 *tgt, integer tCount, uint32 *a, integer aCount, uint32 *b, integer bCount) {
  uint32 comp[bCount];
  longComplement(comp, b, bCount);
  return longAdd(tgt, tCount, a, aCount, comp, bCount);
}

multiPo multiMinus(multiPo lhs, multiPo rhs) {
  integer max = maximum(lhs->size, rhs->size) + 1;
  uint32 sum[max];
  longComplement(sum, rhs->data, rhs->size);
  integer len = longAdd(sum, max, lhs->data, lhs->size, sum, rhs->size);
  return allocMulti(sum, len);
}

static integer mulByAndAdd(uint32 *tgt, uint32 *data, integer len, uint32 factor, uint32 carry) {
  integer ix = 0;
  while (ix < len) {
    uint64 segment = ((uint64) data[ix]) * factor + carry;
    carry = segment >> WIDTH;
    tgt[ix++] = segment & ONES_MASK;
  }
  if (carry > 0)
    tgt[ix++] = carry;
  return ix;
}

multiPo multiFromStr(char *str) {
  return multiFromText(str, uniStrLen(str));
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
      dLen = mulByAndAdd(data, data, dLen, 10, digit);
    } else
      return Null;
  }
  // Allow for case where number might look negative
  dLen++;

  if (positive) {
    return allocMulti(data, dLen);
  } else {
    dLen = longComplement(data, data, dLen);
    return allocMulti(data, dLen);
  }
}

integer longFromText(const char *text, integer tLen, uint32 *data, integer count) {
  zeroFill(data, count);
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
  while (px < tLen) {
    codePoint ch = nextCodePoint(text, &px, tLen);
    if (isNdChar(ch)) {
      byte digit = digitValue(ch);
      dLen = mulByAndAdd(data, data, dLen, 10, digit);
    } else
      return -1;
  }
  // Allow for case where number might look negative
  dLen++;

  if (positive) {
    return longTrim(data, dLen);
  } else {
    return longComplement(data, data, dLen);
  }
}

static integer smallDivide(uint32 *tgt, uint32 *data, integer count, uint32 *last, uint32 base) {
  integer ix = count;
  uint64 work = 0;

  while (ix > 0) {
    work = work << WIDTH | data[ix - 1];
    uint64 q = work / base;
    work = work % base;
    tgt[--ix] = q;
  }

  if (last != Null)
    *last = work;
  return longTrim(tgt, count);
}

static char hxDgit(integer h) {
  if (h < 10)
    return (char) ((unsigned) h | (unsigned) '0');
  else
    return (char) (h + 'a' - 10);
}

static void longTxt(char *text, integer *pos, uint32 *data, integer count, uint8 base) {
  if (count == 1 && data[0] < base) {
    text[(*pos)++] = hxDgit(data[0]);
  } else {
    uint32 last;
    count = smallDivide(data, data, count, &last, base);
    longTxt(text, pos, data, count, base);
    text[(*pos)++] = hxDgit(last);
  }
}

static integer longText(char *out, uint32 *data, integer count, uint8 base, sign *sign) {
  uint32 temp[count];
  integer pos = 0;

  if (longNegative(data, count)) {
    longComplement(temp, data, count);
    *sign = negative;
  } else {
    dataMove(temp, data, count);
    *sign = positive;
  }
  longTxt(out, &pos, temp, count, base);
  return pos;
}

integer textFromlong(char *text, integer tLen, uint32 *data, integer count) {
  assert(tLen >= count * 10);
  uint32 temp[count];
  integer pos = 0;

  if (longNegative(data, count)) {
    longComplement(temp, data, count);
    text[pos++] = '-';
  } else {
    dataMove(temp, data, count);
  }
  longTxt(text, &pos, temp, count, 10);
  return pos;
}

static integer multiText(char *text, integer tLen, multiPo num, uint8 base, sign *sign) {
  integer count = num->size;
  uint32 temp[count];
  integer pos = 0;

  if (multiNegative(num)) {
    longComplement(temp, num->data, count);
    *sign = negative;
  } else {
    dataMove(temp, num->data, count);
    *sign = positive;
  }
  longTxt(text, &pos, temp, count, base);
  return pos;
}

integer longFormat(uint32 *data, integer count, const char *format, integer formatLen, char *buffer, integer buffLen) {
  uint8 base = (uint8) (uniIndexOf(format, formatLen, 0, 'X') >= 0 ? 16 : 10);
  integer precision = uniIndexOf(format, formatLen, 0, '.');
  if (precision < 0)
    precision = formatLen;
  integer outMax = count * INT32_DIGITS;
  char digits[outMax];
  sign sign;
  integer digitLen = longText(digits, data, count, base, &sign);
  integer len = 0;
  formatDigits(sign, digits, digitLen, precision, format, formatLen, buffer, buffLen, &len);

  return len;
}

integer formatMulti(multiPo num, const char *format, integer formatLen, char *buffer, integer buffLen) {
  uint8 base = (uint8) (uniIndexOf(format, formatLen, 0, 'X') >= 0 ? 16 : 10);
  integer precision = uniIndexOf(format, formatLen, 0, '.');
  if (precision < 0)
    precision = formatLen;
  integer count = num->size;
  integer outMax = count * INT32_DIGITS;
  char digits[outMax];
  sign sign;
  integer digitLen = multiText(digits, outMax, num, base, &sign);
  integer len = 0;
  formatDigits(sign, digits, digitLen, precision, format, formatLen, buffer, buffLen, &len);

  return len;
}

retCode showMulti(ioPo out, void *data, long depth, long precision, logical alt) {
  multiPo num = (multiPo) data;
  integer maxDigits = multiSize(num) * INT32_DIGITS + 2;
  char scratch[maxDigits];
  sign sign;

  integer strLen = multiText(scratch, maxDigits, data, 10, &sign);
  return outMsg(out, "%s%.*s", (sign == negative ? "-" : ""), strLen, scratch);
}

retCode showLong(ioPo out, uint32 *digits, long count) {
  char text[count * INT32_DIGITS + 2];
  uint32 scratch[count];

  sign sign;

  if (longNegative(digits, count)) {
    longComplement(scratch, digits, count);
    sign = negative;
  } else {
    dataMove(scratch, digits, count);
    sign = positive;
  }

  integer strLen = 0;
  longTxt(text, &strLen, scratch, count, 10);
  return outMsg(out, "%s%.*s[%d]\n%_", (sign == negative ? "-" : "+"), strLen, text, count);
}

// Assumes both numbers are positive
static integer longMult(uint32 *tgt, uint32 *a, integer aCount, uint32 *b, integer bCount) {
  zeroFill(tgt, aCount + bCount);
  for (integer i = 0; i < aCount; i++) {
    uint64 nA = a[i];
    if (nA != 0) {
      uint32 carry = 0;

      for (integer j = 0; j < bCount; j++) {
        uint64 d = carry + nA * (uint64) b[j] + (uint64) (tgt[i + j]);
        carry = d >> 32;
        tgt[i + j] = d & ONES_MASK;
      }
      tgt[i + bCount] = carry;
    }
  }
  return longTrim(tgt, aCount + bCount);
}

integer longMultiply(uint32 *tgt, integer tSize, uint32 *a, integer aCount, uint32 *b, integer bCount) {
  uint32 bb[bCount];
  uint32 pp[aCount + bCount + 2];

  if (longNegative(a, aCount)) {
    uint32 aa[aCount];
    longComplement(aa, a, aCount);
    if (longNegative(b, bCount)) {
      longComplement(bb, b, bCount);
      return longMult(tgt, aa, aCount, bb, bCount);
    } else {
      dataMove(bb, b, bCount);
      return longComplement(tgt, pp, longMult(pp, aa, aCount, bb, bCount));
    }
  } else {
    if (longNegative(b, bCount)) {
      longComplement(bb, b, bCount);
      return longMult(tgt, a, aCount, bb, bCount);
    } else {
      return longMult(tgt, a, aCount, b, bCount);
    }
  }
}

multiPo multiTimes(multiPo lhs, multiPo rhs) {
  integer rSize = lhs->size + rhs->size;
  uint32 prod[rSize];

  integer pC = longMultiply(prod, rSize, multiData(lhs), multiSize(lhs), multiData(rhs), multiSize(rhs));

  return allocMulti(prod, pC);
}

// Compute left shift
static integer longShiftLeft(uint32 *tgt, integer tC, const uint32 *src, integer sC, uint8 shift) {
  uint64 carry = 0;
  assert(tC > sC);
  for (integer ix = 0; ix < sC; ix++) {
    carry = ((uint64) (src[ix]) << shift) | carry;
    tgt[ix] = (uint32) (carry & ONES_MASK);
    carry >>= WIDTH;
  }
  if (carry != 0) {
    tgt[sC++] = carry;
  }
  return sC;
}

static integer longShiftRight(uint32 *tgt, integer tC, const uint32 *src, integer sC, uint8 shift) {
  uint64 carry = 0;
  assert(tC >= sC);
  for (integer ix = sC - 1; ix >= 0; ix--) {
    carry = ((((uint64) src[ix]) << WIDTH) >> shift) | carry;
    tgt[ix] = (uint32) ((carry >> WIDTH) & ONES_MASK);
    carry <<= WIDTH;
  }

  return sC;
}

// Knuth Algorithm for division
static retCode longDv(uint32 *q, integer *qC, uint32 *r, integer *rC, uint32 *u, integer uC, uint32 *v, integer vC) {
  assert(uC >= vC && vC > 0);

  switch (vC) {
    case 0:
      return Error;
    case 1: {
      *qC = smallDivide(q, u, uC, r, *v);
      *rC = 1;
      return Ok;
    }
    default: {
      uint32 normShift = 31 - lg2(v[vC - 1]); // Normalization factor

      integer uuS = uC + 1;
      integer vvS = vC + 1;
      uint32 uu[uuS];
      uint32 vv[vvS];

      integer vvC = longShiftLeft(vv, vvS, v, vC, normShift);
      integer uuC = longShiftLeft(uu, uuS, u, uC, normShift);

      if (vvC > uuC) {          // Quotient is zero
        *q = 0;
        *qC = 1;
        *rC = uC;
        return wordMove(r, *rC, u, uC); // remainder is dividend
      } else if (vvC == uuC) {  // There will be exactly one quotient digit
        uint32 v1 = vv[vvC - 1];
        uint32 v2 = vvC > 1 ? vv[vvC - 2] : 0;

        uint64 u0 = (uint64) uu[uuC - 1];
        uint64 q0 = u0 / v1;
        uint64 qrem = u0 - ((uint64) q0) * v1;
        uint32 u2 = (uuC > 2 ? uu[uuC - 3] : 0);
        while ((uint64) v2 * q0 > ((qrem << WIDTH) | u2)) {
          q0--;
          qrem++;
        }
        integer tS = uuC + vvC + 1;
        uint32 tmp[tS];
        integer tC = mulByAndAdd(tmp, vv, vvC, q0, 0);
        longSubtract(uu + uuC - tC, tC, uu + uuC - tC, tC, tmp, tC); // little endian math

        if (longNegative(uu + uuC - tC, tC)) {
          longAdd(uu + uuC - tC, tC, uu + uuC - tC, tC, vv, vvC); // add one copy back in
          q0--;
        }

        *q = q0;
        *qC = 1; // Just one quotient digit
        *rC = longShiftRight(r, *rC, uu, vC, normShift);
        return Ok;
      } else {
        uint32 v1 = vv[vvC - 1];
        uint32 v2 = vvC > 1 ? vv[vvC - 2] : 0;

        integer qP = uuC - vvC;
        for (integer uJ = uuC; uJ > vvC; uJ--) {
          uint64 u0 = (((uint64) uu[uJ - 1]) << WIDTH) | uu[uJ - 2];
          uint64 q0 = u0 / v1;
          uint64 qrem = u0 - ((uint64) q0) * v1;
          uint32 u2 = (uJ > 2 ? uu[uJ - 3] : 0);
          while ((uint64) v2 * q0 > ((qrem << WIDTH) | u2)) {
            q0--;
            qrem++;
          }
          integer tS = uJ + vvC + 1;
          uint32 tmp[tS];
          integer tC = mulByAndAdd(tmp, vv, vvC, q0, 0);
          longSubtract(uu + uJ - tC, tC, uu + uJ - tC, tC, tmp, tC); // little endian math

          if (longNegative(uu + uJ - tC, tC)) {
            longAdd(uu + uJ - tC, tC, uu + uJ - tC, tC, vv, vvC); // add one copy back in
            q0--;
          }

          q[--qP] = q0; // putting quotient in from the most significant first
        }

        *qC = uuC - vvC; // This is how many digits of quotient we wrote
        // wordReverse(q, qP);
        *rC = longShiftRight(r, *rC, uu, vC, normShift);
        return Ok;
      }
    }
  }
}

retCode longDivide(uint32 *q, integer *qC, uint32 *r, integer *rC, uint32 *n, integer nC, uint32 *d, integer dC) {
  if (longIsZero(d, dC))
    return Error;
  else if (longNegative(n, nC)) {
    uint32 nn[nC + 1];
    integer nnC = longComplement(nn, n, nC);

    if (longNegative(d, dC)) {
      uint32 dd[dC + 1];
      integer ddC = longComplement(dd, d, dC);
      return longDv(q, qC, r, rC, nn, nnC, dd, ddC);
    } else {
      integer qqC = nC + dC + 1;
      uint32 qq[qqC];
      integer rrC = nC + dC + 1;
      uint32 rr[rrC];

      longDv(qq, &qqC, rr, &rrC, nn, nnC, d, dC);
      *qC = longComplement(q, qq, qqC);
      *rC = longComplement(r, rr, rrC);
      return Ok;
    }
  } else if (longNegative(d, dC)) {
    integer qqC = nC + dC + 1;
    uint32 qq[qqC];
    integer rrC = nC + dC + 1;
    uint32 rr[rrC];

    longDv(qq, &qqC, rr, &rrC, n, nC, d, dC);
    *qC = longComplement(q, qq, qqC);
    *rC = longComplement(r, rr, rrC);
    return Ok;
  } else {
    return longDv(q, qC, r, rC, n, nC, d, dC);
  }
}

retCode multiDivide(multiPo *quot, multiPo *rem, multiPo lhs, multiPo rhs) {
  integer qC = 2 * (multiSize(lhs) + multiSize(rhs) + 1); // The intermediate numbers can get large
  uint32 q[qC];
  uint32 r[qC];
  integer qS = qC, rS = qC;

  longDivide(q, &qS, r, &rS, multiData(lhs), multiSize(lhs), multiData(rhs), multiSize(rhs));
  *quot = allocMulti(q, qS);
  *rem = allocMulti(r, rS);
  return Ok;
}

integer longGCD(uint32 *tgt, uint32 *a, integer aC, uint32 *b, integer bC) {
  if (longCompare(a, aC, b, bC) == bigger) {
    uint32 q[aC];
    uint32 r[aC];
    integer qC = aC;
    integer rC = aC;
    longDv(q, &qC, r, &rC, a, aC, b, bC);
    if (longIsZero(r, rC)) {
      dataMove(tgt, b, bC);
      return bC;
    } else
      return longGCD(tgt, b, bC, r, rC);
  } else {
    uint32 q[bC];
    uint32 r[bC];
    integer qC = bC;
    integer rC = bC;
    longDv(q, &qC, r, &rC, b, bC, a, aC);
    if (longIsZero(r, rC)) {
      dataMove(tgt, a, aC);
      return aC;
    } else
      return longGCD(tgt, a, aC, r, rC);
  }
}

multiPo multiGCD(multiPo a, multiPo b) {
  uint32 gcd[multiSize(a) + multiSize(b) + 1];
  integer ggC = longGCD(gcd, multiData(a), multiSize(a), multiData(b), multiSize(b));
  return allocMulti(gcd, ggC);
}

uinteger longHash(uint32 *data, integer count) {
  uinteger h = 0;
  integer chLen = count * INT32_DIGITS + 1;
  char chs[chLen];
  for (integer ix = 0; ix < count; ix++) {
    h = h * 37 + (uinteger) data[ix];
  }
  return hash64(h);
}
