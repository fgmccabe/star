//
// Created by Francis McCabe on 9/21/20.
//

#include "ltype.h"

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
      case vdTp:
        return Ok;
      case parTp: {
        nextCodePoint(text, pos, len);
        return Ok;
      }
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

retCode typeSigArity(const char *sig, integer len, int32 *arity) {
  if (*sig == tplTp) {
    integer pos = 1;
    *arity = 0;
    while (sig[pos] != ')') {
      (*arity)++;
      tryRet(skipTypeSig(sig, len, &pos));
    }
    return Ok;
  } else
    return Error;
}

retCode funArgSig(const char *text, integer len, integer *pos) {
  codePoint ch = nextCodePoint(text, pos, len);
  if (ch == funTp)
    return Ok;
  else
    return Error;
}

retCode funResSig(const char *text, integer len, integer *pos) {
  codePoint ch = nextCodePoint(text, pos, len);
  if (ch == funTp)
    return skipTypeSig(text, len, pos);
  else
    return Error;
}

logical isTupleSig(const char *text, integer len) {
  if (len > 0) {
    integer pos = 0;
    codePoint ch = nextCodePoint(text, &pos, len);
    return ch == tplTp;
  } else
    return False;
}

static retCode showSig(ioPo out, const char *sig, integer *pos, integer len);

retCode showLSig(ioPo out, const char *sig, integer len) {
  integer pos = 0;
  return showSig(out, sig, &pos, len);
}

retCode showLS(ioPo f, void *data, long depth, long precision, logical alt) {
  char *sig = (char *) data;
  integer pos = 0;
  return showSig(f, sig, &pos, depth);
}

retCode showSig(ioPo out, const char *sig, integer *pos, integer len) {
  codePoint ch = nextCodePoint(sig, pos, len);
  if (ch != 0) {
    switch (ch) {
      case int64Tp:
        return outStr(out, "i");
      case flt64Tp:
        return outStr(out, "f");
      case boolTp:
        return outStr(out, "b");
      case ptrTp:
        return outStr(out, "p");
      case vdTp:
        return outStr(out, "v");
      case parTp: {
        ch = nextCodePoint(sig, pos, len);
        return outMsg(out, "\\%%c", ch);
      }
      case funTp: {
        retCode ret = showSig(out, sig, pos, len);
        if (ret == Ok)
          ret = outStr(out, "=>");
        if (ret == Ok)
          ret = showSig(out, sig, pos, len);
        return ret;
      }
      case tplTp: {
        retCode ret = outStr(out, "(");
        char *sep = "";
        while (ret == Ok) {
          integer ps = *pos;
          if (nextCodePoint(sig, &ps, len) == ')') {
            *pos = ps;
            return outStr(out, ")");
          } else
            outStr(out, sep);
          ret = showSig(out, sig, pos, len);
          sep = ",";
        }
        return Ok;
      }
      default:
        return Error;
    }
  } else
    return Eof;
}
