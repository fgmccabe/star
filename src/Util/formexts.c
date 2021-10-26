//
// Created by Francis McCabe on 1/14/18.
//

#include "formioP.h"

static retCode quoteChar(ioPo f, codePoint ch) {
  retCode ret;
  switch (ch) {
    case '\a':
      ret = outStr(f, "\\a");
      break;
    case '\b':
      ret = outStr(f, "\\b");
      break;
    case '\x7f':
      ret = outStr(f, "\\d");
      break;
    case '\x1b':
      ret = outStr(f, "\\e");
      break;
    case '\f':
      ret = outStr(f, "\\f");
      break;
    case '\n':
      ret = outStr(f, "\\n");
      break;
    case '\r':
      ret = outStr(f, "\\r");
      break;
    case '\t':
      ret = outStr(f, "\\t");
      break;
    case '\v':
      ret = outStr(f, "\\v");
      break;
    case '\\':
      ret = outStr(f, "\\\\");
      break;
    case '\'':
      ret = outStr(f, "\\\'");
      break;
    case '`':
      ret = outStr(f, "\\`");
      break;
    case '\"':
      ret = outStr(f, "\\\"");
      break;
    default:
      if (ch < ' ') {
        ret = outChar(f, '\\');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 6u) & 3u) | (unsigned) '0');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 3u) & 7u) | (unsigned) '0');
        if (ret == Ok)
          ret = outChar(f, (ch & 7u) | (unsigned) '0');
      } else
        ret = outChar(f, ch);
  }
  return ret;
}

retCode genQuotedChr(ioPo f, void *data, long depth, long precision, logical alt) {
  codePoint cp = (codePoint) (integer) data;

  return quoteChar(f, cp);
}

retCode genQuotedStr(ioPo f, void *data, long depth, long precision, logical alt) {
  char *txt = (char *) data;
  integer len = (integer) uniStrLen(txt);
  integer pos = 0;

  retCode ret = Ok;
  while (ret == Ok && pos < len) {
    codePoint cp = nextCodePoint(txt, &pos, len);
    ret = quoteChar(f, cp);
  }
  return ret;
}

static logical needsQuote(codePoint ch) {
  switch (ch) {
    case '\a':
    case '\b':
    case '\x7f':
    case '\x1b':
    case '\f':
    case '\n':
    case '\r':
    case '\t':
    case '\v':
    case '\\':
    case '\"':
      return True;
    default:
      if (ch < ' ' || ch >= 128)
        return True;
      else
        return False;
  }
}

logical needQuoting(char *str, integer len) {
  integer pos = 0;
  while (pos < len) {
    codePoint ch = nextCodePoint(str, &pos, len);
    if (needsQuote(ch))
      return True;
  }
  return False;
}
