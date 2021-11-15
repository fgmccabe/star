//
// Created by Francis McCabe on 11/12/21.
//

#include <stdlib.h>
#include "bcdP.h"
#include "assert.h"

#define hiNibble(X) (((X)>>4)&0xf)
#define loNibble(X) ((X)&0xf)

static uint8 getDigit(const byte *data, integer cnt) {
  integer ix = cnt / 2;
  if (isEven(cnt))
    return hiNibble(data[ix]);
  else
    return loNibble(data[ix]);
}

static void setDigit(byte *data, integer off, byte val) {
  integer ix = off / 2;
  byte curr = data[ix];
  if (isOdd(off)) {
    data[ix] = (curr & 0xf0) | (val & 0xf);
  } else {
    data[ix] = (curr & 0xf) | ((val & 0xf) << 4);
  }
}

static void zeroFill(byte *data, integer count) {
  for (integer ix = 0; ix < ALIGNVALUE(count, 2); ix++) {
    data[ix] = 0;
  }
}

static integer bcdTrim(byte *data, integer len) {
  while (len > 1 && getDigit(data, len - 1) == 0)
    len--;
  return len;
}

// Slow, but sure.
static void bcdMove(byte *tgt, byte *data, integer count) {
  for (integer ix = 0; ix < count; ix++) {
    setDigit(tgt, ix, getDigit(data, ix));
  }
}

bcdPo allocBCD(sign sign, integer count, byte *data) {
  count = bcdTrim(data, count);

  bcdPo bcd = (bcdPo) malloc(sizeof(BcdNumberRecord) + ALIGNVALUE(count, 2));
  bcd->sign = sign;
  bcd->count = count;
  bcdMove(bcd->data, data, count);
  return bcd;
}

integer textOfBCD(char *text, integer tlen, bcdPo num) {
  integer pos = 0;
  switch (num->sign) {
    case positive:
      text[pos++] = '+';
      break;
    case negative:
      text[pos++] = '-';
      break;
  }

  for (integer ix = num->count - 1; ix >= 0; ix--) {
    uint8 nibble = getDigit(num->data, ix);
    text[pos++] = "0123456789"[nibble];
  }
  return pos;
}

bcdPo bcdFromText(char *text, integer tlen) {
  integer px = 0;
  sign sign = positive;
  byte data[tlen];

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
  integer ix = 0;
  while (txpos > px) {
    codePoint ch = prevCodePoint(text, &txpos, tlen);
    if (isNdChar(ch)) {
      int digit = digitValue(ch);
      setDigit(data, ix++, digit);
    }
  }

  return allocBCD(sign, ix, data);
}

logical sameBCD(bcdPo lhs, bcdPo rhs) {
  integer l1 = lhs->count;
  if (l1 == rhs->count && lhs->sign == rhs->sign) {
    byte *d1 = lhs->data;
    byte *d2 = rhs->data;
    for (integer ix = 0; ix < l1; ix++)
      if (getDigit(d1, ix) != getDigit(d2, ix))
        return False;
    return True;
  }
  return False;
}

void trimBCD(bcdPo bcd) {
  integer len = bcd->count;
  while (len >= 0 && getDigit(bcd->data, len - 1) == 0)
    len--;
  bcd->count = len;
}

static integer bcdSum(byte lhs[], integer llen, byte rhs[], integer rlen, byte sum[], integer slen) {
  assert(slen > llen && slen > rlen);

  uint8 carry = 0;
  integer sp = 0;

  for (integer lp = 0, rp = 0; lp < llen && rp < rlen;) {
    uint8 digit = (lp < llen ? getDigit(lhs, lp++) : 0) + (rp < rlen ? getDigit(rhs, rp++) : 0) + carry;
    if (digit > 9) {
      digit = digit - 10;
      carry = 1;
    } else
      carry = 0;
    setDigit(sum, sp++, digit);
  }

  return sp;
}

static integer bcdSubtract(byte lhs[], integer llen, byte rhs[], integer rlen, byte reslt[], integer slen, sign *sign) {
  assert(slen > llen && slen > rlen);

  uint8 borrow = 0;
  integer sp = 0;

  for (integer lp = 0, rp = 0; lp < llen && rp < rlen;) {
    int digit = (int8) (lp < llen ? getDigit(lhs, lp++) : 0) - (int8) (rp < rlen ? getDigit(rhs, rp++) : 0) - borrow;
    if (digit < 0) {
      digit = digit + 10;
      borrow = 1;
    } else
      borrow = 0;
    setDigit(reslt, sp++, digit);
  }

  if (borrow == 0)
    *sign = positive;
  else
    *sign = negative;

  return sp;
}

bcdPo bcdPlus(bcdPo lhs, bcdPo rhs) {
  integer rsize = maximum(lhs->count, rhs->count) + 1;
  byte data[rsize];

  switch (lhs->sign) {
    case positive: {
      switch (rhs->sign) {
        case positive: {
          integer len = bcdSum(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize);
          return allocBCD(positive, len, data);
        }
        case negative: {
          sign sign = positive;
          integer len = bcdSubtract(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize, &sign);
          return allocBCD(sign, len, data);
        }
      }
    }
    case negative: {
      switch (rhs->sign) {
        case positive: {
          sign sign = positive;
          integer len = bcdSubtract(rhs->data, rhs->count, lhs->data, lhs->count, data, rsize, &sign);
          return allocBCD(sign, len, data);
        }
        case negative: {
          integer len = bcdSum(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize);
          return allocBCD(negative, len, data);
        }
      }
    }
  }
}

bcdPo bcdMinus(bcdPo lhs, bcdPo rhs) {
  integer rsize = maximum(lhs->count, rhs->count) + 1;
  byte data[rsize];

  switch (lhs->sign) {
    case positive: {
      switch (rhs->sign) {
        case positive: {
          sign sign = positive;
          integer len = bcdSubtract(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize, &sign);
          return allocBCD(sign, len, data);
        }
        case negative: {
          integer len = bcdSum(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize);
          return allocBCD(positive, len, data);
        }
      }
    }
    case negative: {
      switch (rhs->sign) {
        case positive: {
          integer len = bcdSum(lhs->data, lhs->count, rhs->data, rhs->count, data, rsize);
          return allocBCD(negative, len, data);
        }
        case negative: {
          sign sign = positive;
          integer len = bcdSubtract(rhs->data, rhs->count, lhs->data, lhs->count, data, rsize, &sign);
          return allocBCD(sign, len, data);
        }
      }
    }
  }
}

integer bcdMul(byte lhs[], integer llen, byte rhs[], integer rlen, byte prod[], integer plen) {
  zeroFill(prod, plen);
  integer pxlen = 0;

  for (integer lp = 0; lp < llen; lp++) {
    byte scratch[rlen];
    uint ldigit = getDigit(lhs, lp);

    uint8 carry = 0;
    integer sp = 0;
    for (integer rp = 0; rp < rlen; rp++) {
      uint8 digit = getDigit(rhs, rp) * ldigit + carry;
      if (digit > 10) {
        carry = digit / 10;
        digit = digit % 10;
      } else
        carry = 0;
      setDigit(scratch, sp++, digit);
    }
    while (carry > 0) {
      if (carry > 10) {
        setDigit(scratch, sp++, carry % 10);
        carry = carry / 10;
      } else
        carry = 0;
    }

    pxlen = bcdSum(prod, pxlen, scratch, sp, prod, pxlen);

  }
}

retCode fillinBCD(sign sign, integer count, integer size, byte *data, bcdPo bcd) {
  if (count > size)
    return Error;
  else {
    bcd->sign = sign;
    bcd->count = count;
    bcdMove(bcd->data, data, count);
    return Ok;
  }
}
