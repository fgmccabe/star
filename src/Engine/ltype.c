//
// Created by Francis McCabe on 9/21/20.
//

#include "ltype.h"

static retCode decNat(const char *text, integer len, integer *pos, integer *ii);

retCode validTypeSig(const char *text, integer len) {
  integer pos = 0;
  retCode ret = skipTypeSig(text, len, &pos);
  if (ret == Ok && pos == len)
    return Ok;
  else
    return Error;
}

retCode skipTypeSig(const char *text, integer len, integer *pos) {
  codePoint ch = nextCodePoint(text, pos, len);
  if (ch != 0) {
    switch (ch) {
      case int64Tp:
      case flt64Tp:
      case boolTp:
      case ptrTp:
        return Ok;
      case funTp: {
        retCode ret = skipTypeSig(text, len, pos);
        if (ret == Ok)
          ret = skipTypeSig(text, len, pos);
        return ret;
      }
      case tplTp: {
        retCode ret = Ok;
        while (ret == Ok) {
          integer ps = *pos;
          if (nextCodePoint(text, &ps, len) == ')') {
            *pos = ps;
            return Ok;
          } else
            ret = skipTypeSig(text, len, pos);
        }
        return Ok;
      }
      default:
        return Error;
    }
  } else
    return Eof;
}

static logical isDigit(codePoint ch) {
  return (logical) (ch >= '0' && ch <= '9');
}

static int digitVal(codePoint ch) {
  return (int) (ch - '0');
}

retCode decNat(const char *text, integer len, integer *pos, integer *ii) {
  integer result = 0;
  integer ps = *pos;
  codePoint ch;

  while (isDigit(ch = nextCodePoint(text, &ps, len))) {
    result = result * 10 + digitVal(ch);
    *pos = ps;
  }
  *ii = result;
  return Ok;
}
