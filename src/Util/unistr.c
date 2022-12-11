/*
  Unicode encoding and decoding functions
  Copyright (c) 2016, 2017. Francis G. McCabe
*/

#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include "unistrP.h"


char* retCodeNames[MaxRetCode] = {"Ok", "Fail", "Switch", "Interrupt", "Error", "Eof", "Space"};

retCode nxtPoint(const char *src, integer *start, integer end, codePoint *code) {
  integer pos = *start;

  if (pos < end) {
    unsigned char b = (unsigned char) src[pos++];

    if (b <= 0x7f) {
      *code = (codePoint) b;
      *start = pos;
      return Ok;
    } else if (UC80(b)) {
      if (pos < end) {
        unsigned char nb = (unsigned char) src[pos++];
        codePoint ch = (codePoint) (UX80(b) << 6u | UXR(nb));

        if (ch < 0x7ff) {
          *code = ch;
          *start = pos;
          return Ok;
        } else
          return Error;
      } else
        return Eof;
    } else if (UC800(b)) {
      if (pos + 1 < end) {
        unsigned char md = (unsigned char) src[pos++];
        unsigned char up = (unsigned char) src[pos++];

        codePoint ch = (codePoint) ((UX800(b) << 12u) | (UXR(md) << 6u) | (UXR(up)));

        if (ch >= 0x800 && ch <= 0xffff) {
          *code = ch;
          *start = pos;
          return Ok;
        } else
          return Error;
      } else
        return Eof;
    } else
      return Error;
  } else
    return Eof;
}

retCode prevPoint(const char *src, integer *start, codePoint *code) {
  long pos = *start;

  if (pos > 0) {
    unsigned char b = (unsigned char) src[--pos];

    if (b <= 0x7f) {
      *code = (codePoint) b;
      *start = pos;
      return Ok;
    } else {
      codePoint pt = 0;
      unsigned int factor = 0;
      while (UCR(b)) {
        pt = pt | (UXR(b) << factor);
        factor += 6;
        b = (unsigned char) src[--pos];
      }
      if (UC80(b)) {
        *code = pt | (UX80(b) << factor);
        *start = pos;
        return Ok;
      } else if (UC800(b)) {
        *code = pt | (UX800(b) << factor);
        *start = pos;
        return Ok;
      } else
        return Error;
    }
  } else
    return Eof;
}

integer backCodePoint(char *src, integer from, integer count) {
  while (count-- > 0 && from > 0) {
    codePoint ch;
    if (prevPoint(src, &from, &ch) == Ok)
      continue;
    else
      return -1;
  }
  return from;
}

integer countCodePoints(const char *src, integer start, integer end) {
  integer count = 0;

  while (start < end) {
    codePoint ch;

    if (nxtPoint(src, &start, end, &ch) == Ok)
      count++;
    else
      return count;
  }
  return count;
}

int codePointSize(codePoint ch) {
  if (ch > 0 && ch <= 0x7f)
    return 1;
  else if (ch <= 0x7ff)
    return 2;
  else if (ch >= 0x800 && ch <= 0xffff)
    return 3;
  else
    return 4;
}

integer advanceCodePoint(char *src, integer start, integer end, integer count) {
  while (count-- > 0 && start < end) {
    codePoint ch;
    if (nxtPoint(src, &start, end, &ch) == Ok)
      continue;
    else
      return -1;
  }
  return start;
}

codePoint nextCodePoint(const char *src, integer *start, integer end) {
  codePoint ch;
  if (nxtPoint(src, start, end, &ch) == Ok)
    return ch;
  else
    return (codePoint) 0;
}

codePoint prevCodePoint(const char *src, integer *start, integer end) {
  codePoint ch;
  if (prevPoint(src, start, &ch) == Ok)
    return ch;
  else
    return (codePoint) 0;
}

codePoint codePointAt(const char *src, integer pt, integer end) {
  codePoint ch;
  if (nxtPoint(src, &pt, end, &ch) == Ok)
    return ch;
  else
    return (codePoint) 0;
}

integer uniStrLen(const char *s) {
  integer count = 0;
  while (*s++ != 0)
    count++;
  return count;
}

integer uniNStrLen(const char *s, integer max) {
  integer count = 0;
  for (integer ix = 0; ix < max; ix++) {
    if (*s++ == 0)
      return count;
    else
      count++;
  }
  return count;
}

logical isUniIdentifier(char *str, integer len) {
  integer pos = 0;
  while (pos < len) {
    codePoint ch;
    if (nxtPoint(str, &pos, len, &ch) == Ok) {
      if (!isIdContinue(ch))
        return False;
    } else
      return False;
  }
  return True;
}

integer uniByteLen(const char *s) {
  integer len = 0;
  char *p = (char *) s;

  assert(s != NULL);

  while (*p++ != 0)
    len++;
  return len;
}

retCode uniAppend(char *dest, integer *pos, integer len, char *src) {
  for (; *src != 0 && *pos < len;)
    dest[(*pos)++] = *src++;
  if (*pos < len - 1) {
    dest[*pos] = 0;
    return Ok;
  } else {
    return Eof;
  }
}

retCode uniNAppend(char *dest, integer *pos, integer len, char *src, integer sLen) {
  for (long sx = 0; sx < sLen && *pos < len;)
    dest[(*pos)++] = *src++;
  if (*pos < len - 1) {
    dest[*pos] = 0;
    return Ok;
  } else {
    return Eof;
  }
}

retCode appendCodePoint(char *dest, integer *pos, integer len, codePoint ch) {
  if (ch <= 0x7f) {
    if ((*pos) < len - 1) {
      dest[(*pos)++] = (char) ((ch) & 0x7fu);
      return Ok;
    } else
      return Eof;
  } else if (ch <= 0x7ff) {
    if ((*pos) < len - 2) {
      dest[(*pos)++] = (char) ((((ch) >> 6u) & 0x1fu) | U80);
      dest[(*pos)++] = (char) (UXR(ch) | UR);
      return Ok;
    } else
      return Eof;
  } else if (ch >= 0x800 && ch <= 0xffff) {
    if ((*pos) < len - 3) {
      dest[(*pos)++] = (char) ((((ch) >> 12u) & 0xfu) | U800);
      dest[(*pos)++] = (char) (UXR(ch >> 6u) | UR);
      dest[(*pos)++] = (char) (UXR(ch) | UR);
      return Ok;
    } else
      return Eof;
  } else if (ch >= 0x10000 && ch <= 0x1fffff) {
    if ((*pos) < len - 4) {
      dest[(*pos)++] = (char) ((((ch) >> 18u) & 0xfu) | U1000);
      dest[(*pos)++] = (char) (UXR(ch >> 12u) | UR);
      dest[(*pos)++] = (char) (UXR(ch >> 6u) | UR);
      dest[(*pos)++] = (char) (UXR(ch) | UR);
      return Ok;
    } else
      return Eof;
  } else
    return Error;
}

retCode uniReverse(char *dest, integer len) {
  for (integer ix = 0; ix < len / 2; ix++) {
    char b = dest[ix];
    dest[ix] = dest[len - ix - 1];
    dest[len - ix - 1] = b;
  }
  return Ok;
}

retCode processUnicodes(const char *text, integer length, uniCodeProc p, void *cl) {
  retCode ret = Ok;
  integer ix = 0;

  while (ret == Ok && ix < length) {
    codePoint cp;

    integer i = ix;
    ret = nxtPoint(text, &ix, length, &cp);

    if (ret == Ok)
      ret = p(cp, i, cl);
  }
  return ret;
}

retCode uniCpy(char *dest, integer len, const char *src) {
  int pos = 0;
  char *s = (char *) src;

  while (pos < len - 1 && *src != 0)
    dest[pos++] = *s++;
  dest[pos] = 0;
  return pos < len ? Ok : Eof;
}

retCode uniNCpy(char *dest, integer len, const char *src, integer sLen) {
  integer pos = 0;
  integer max = (sLen < len - 1 ? sLen : len - 1);
  char *s = (char *) src;

  while (pos < max && *src != 0)
    dest[pos++] = *s++;
  dest[pos] = 0;
  return pos < len ? Ok : Eof;
}

retCode uniMove(char *dest, integer len, const char *src, integer sLen) {
  integer pos = 0;
  integer max = (sLen < len ? sLen : len);
  char *s = (char *) src;

  while (pos < max)
    dest[pos++] = *s++;
  return pos < len ? Ok : Eof;
}

retCode byteMove(byte *dest, integer len, const byte *src, integer sLen) {
  integer pos = 0;
  integer max = (sLen < len ? sLen : len);
  byte *s = (byte *) src;

  while (pos < max)
    dest[pos++] = *s++;
  return pos <= len ? Ok : Eof;
}

retCode wordMove(uint32 *dest, uint32 len, const uint32 *s, uint32 sLen) {
  integer pos = 0;
  integer max = (sLen < len ? sLen : len);

  while (pos < max)
    dest[pos++] = *s++;
  return pos <= len ? Ok : Eof;
}

logical sameBytes(const byte *s1, integer l1, const byte *s2, integer l2) {
  if (l1 != l2)
    return False;
  else {
    for (integer ix = 0; ix < l1; ix++) {
      if (s1[ix] != s2[ix])
        return False;
    }
    return True;
  }
}

logical sameWords(const uint32 *s1, integer l1, const uint32 *s2, integer l2) {
  if (l1 != l2)
    return False;
  else {
    for (integer ix = 0; ix < l1; ix++) {
      if (s1[ix] != s2[ix])
        return False;
    }
    return True;
  }
}

comparison uniCmp(const char *s1, const char *s2) {
  long pos = 0;
  assert(s1 != NULL && s2 != NULL);

  while (s1[pos] == s2[pos]) {
    if (s1[pos] == 0)
      return same;
    pos++;
  }

  if ((unsigned) s1[pos] < (unsigned) s2[pos] || s1[pos] == 0)
    return smaller;
  else
    return bigger;
}

comparison unicodeCmp(const char *s1, integer l1, const char *s2, integer l2) {
  integer p1 = 0, p2 = 0;
  assert(s1 != NULL && s2 != NULL);

  while (p1 < l1 && p2 < l2) {
    codePoint ch1 = nextCodePoint(s1, &p1, l1);
    codePoint ch2 = nextCodePoint(s2, &p2, l2);

    if (ch1 == ch2)
      continue;
    else if (ch1 < ch2)
      return smaller;
    else
      return bigger;
  }
  if (p1 == l1 && p2 == l2)
    return same;
  else if (p1 < l1)
    return bigger;
  else
    return smaller;
}

comparison uniNCmp(const char *s1, integer l1, const char *s2, integer l2) {
  integer sz = minimum(l1, l2);

  for (integer ix = 0; ix < sz; ix++) {
    if (s1[ix] < s2[ix])
      return smaller;
    else if (s1[ix] > s2[ix])
      return bigger;
  }
  if (l1 < l2)
    return smaller;
  else if (l1 > l2)
    return bigger;
  else
    return same;
}

logical uniSame(const char *s1, integer l1, const char *s2, integer l2) {
  integer ix = 0;
  integer jx = 0;
  while (ix < l1 && jx < l2) {
    codePoint ch1 = nextCodePoint(s1, &ix, l1);
    codePoint ch2 = nextCodePoint(s2, &jx, l2);
    if (ch1 != ch2)
      return False;
  }
  return (logical) (ix == l1 && jx == l2);
}

integer uniIndexOf(const char *s, integer len, integer from, codePoint c) {
  integer pos = from;

  while (pos < len) {
    codePoint ch;
    from = pos;
    if (nxtPoint(s, &pos, len, &ch) == Ok) {
      if (ch == c)
        return from;
    }
  }
  return -1;
}

integer uniLastIndexOf(char *s, integer len, codePoint c) {
  integer lx = -1;
  integer pos = 0;

  while (pos < len) {
    codePoint ch;
    integer nxt = pos;
    if (nxtPoint(s, &nxt, len, &ch) == Ok) {
      if (ch == c) {
        lx = pos;
      }
      pos = nxt;
    }
  }
  return lx;
}

codePoint uniSearchDelims(char *s, integer len, char *t) {
  integer tlen = uniStrLen(t);
  integer tSize = countCodePoints(t, 0, tlen);

  codePoint terms[tSize];
  integer ti = 0;

  for (integer tx = 0; tx < tSize;) {
    terms[ti++] = nextCodePoint(t, &tx, tSize);
  }

  for (integer ix = 0; ix < len;) {
    codePoint ch = nextCodePoint(s, &ix, len);
    for (long dx = 0; dx < tSize; dx++) {
      if (terms[dx] == ch) {
        terms[dx] = 0;
        break;
      }
    }
  }

  for (long dx = 0; dx < tSize; dx++) {
    if (terms[dx] != 0)
      return terms[dx];
  }
  return 0;
}

// This is a poor algorithm. Fix me with Boyer-Moore or better
long uniSearch(const char *src, integer len, integer start, const char *tgt, integer tlen) {
  long pos = start;

  while (pos < len - tlen) {
    if (uniIsPrefix(tgt, tlen, &src[pos], len - pos))
      return pos;
    else
      pos++;
  }
  return -1;
}

logical uniIsTrivial(const char *s, integer len) {
  integer pos = 0;
  while (pos < len) {
    codePoint ch = nextCodePoint(s, &pos, len);
    if (!isSpaceChar(ch))
      return False;
  }
  return True;
}

retCode uniTrim(const char *s, integer sLen, char *front, char *trail, char *out, integer outLen) {
  const integer frSize = uniStrLen(front);
  const integer trSize = uniStrLen(trail);

  integer fx = 0;
  integer tx = sLen;

  while (fx < sLen) {
    integer px = fx;
    codePoint ch = nextCodePoint(s, &px, sLen);
    if (uniIndexOf(front, frSize, 0, ch) >= 0) {
      fx = px;
      continue;
    } else
      break;
  }

  while (tx >= fx) {
    codePoint ch;
    integer px = tx;
    if (prevPoint(s, &px, &ch) == Ok) {
      if (uniIndexOf(trail, trSize, 0, ch) >= 0) {
        tx = px;
        continue;
      } else
        break;
    } else
      break;
  }

  return uniNCpy(out, outLen, &s[fx], tx - fx);
}

logical uniIsLit(const char *s1, const char *s2) {
  long pos = 0;
  while (s2[pos] != 0 && s1[pos] == s2[pos])
    pos++;

  return (logical) (s2[pos] == 0 && s1[pos] == 0);
}

logical uniIsLitPrefix(const char *s1, const char *s2) {
  long pos = 0;
  while (s2[pos] != '\0' && s1[pos] == s2[pos])
    pos++;

  return (logical) (s2[pos] == 0);
}

logical uniIsPrefix(const char *s1, integer len1, const char *s2, integer len2) {
  long pos = 0;

  while (pos < len1 && pos < len2 && s1[pos] == s2[pos])
    pos++;

  return (logical) (len1 <= len2 && pos == len1);
}

integer uniHash(const char *name) {
  return uniNHash(name, uniByteLen((name)));
}

integer uniNHash(const char *name, long len) {
  register integer hash = 0;
  integer fx = 0;

  while (fx < len) {
    codePoint ch = nextCodePoint(name, &fx, len);
    hash = hash * 37 + ch;
  }

  return hash61(hash);
}

integer byteHash(const byte *data, long len) {
  register integer hash = 0;
  integer fx = 0;

  while (fx < len) {
    hash = hash * 37 + data[fx++];
  }

  return hash61(hash);
}

integer wordHash(const uint32 *data, long len) {
  register integer hash = 0;
  integer fx = 0;

  while (fx < len) {
    hash = hash * 37 + data[fx++];
  }

  return hash61(hash);
}

// Ensures that hash codes are always positive
integer hash61(integer ix){
  return (integer) ((uint64) ix & ((uint64) LARGE_INT61));
}

retCode uniLower(const char *s, integer sLen, char *d, integer dLen) {
  integer sPos = 0;
  integer dPos = 0;

  while (sPos < sLen && dPos < dLen) {
    codePoint ch;
    if (nxtPoint(s, &sPos, sLen, &ch) == Ok) {
      appendCodePoint(d, &dPos, dLen, lowerOf(ch));
    } else
      return Error;
  }
  if (dPos < dLen - 1) {
    d[dPos] = 0;
    return Ok;
  } else
    return Eof;
}

char *uniDuplicate(const char *s) {
  size_t len = (size_t) uniStrLen(s) + 1;
  char *copy = (char *) malloc(len * sizeof(byte));

  memcpy(copy, s, len);
  return copy;
}

char *uniDupl(char *str, integer len) {
  char *copy = (char *) malloc(len * sizeof(char));
  memcpy(copy, str, len);
  return copy;
}

void uniDestroy(char *s) {
  free(s);
}
