//
// Created by Francis McCabe on 1/13/18.
//
#include "config.h"
#include <ooio.h>
#include "encoding.h"
#include "signature.h"


retCode encodeInt(ioPo out, int64 ix) {
  tryRet(outChar(out, intTrm));
  return outInt(out, ix);
}

retCode encodeStr(ioPo out, char *str, integer len) {
  tryRet(outChar(out, strTrm));
  return encodeTxt(out, str, len);
}

static codePoint findDelim(char *str, char *choices, codePoint deflt) {
  integer chPos = 0;
  integer choiceLen = uniStrLen(choices);
  integer srcLen = uniStrLen(str);

  while (chPos < choiceLen) {
    codePoint ch = nextCodePoint(choices, &chPos, choiceLen);
    int64 pos = uniIndexOf(str, srcLen, 0, ch);

    if (pos < 0)
      return ch; // We found our delimiter
  }
  return deflt;
}

retCode encodeTxt(ioPo out, char *str, integer len) {
  codePoint delim = findDelim(str, "'\"|/%", '\"');
  codePoint ch;
  retCode ret = outChar(out, delim);

  for (integer pos = 0; ret == Ok && pos < len;) {
    ch = nextCodePoint(str, &pos, len);
    if (ch == delim || ch == '\\') {
      ret = outChar(out, '\\');
    }
    if (ret == Ok)
      ret = outChar(out, ch);
  }

  if (ret == Ok)
    ret = outChar(out, delim);
  return ret;
}

retCode encodeEnum(ioPo out, char *nm){
  return encodeStrct(out,nm,0);
}

retCode encodeFlt(ioPo out, double dx) {
  tryRet(outChar(out,fltTrm));
  return outFloat(out, dx);
}

retCode encodeStrct(ioPo out, char *sx, integer ar) {
  tryRet(outChar(out, lblTrm));
  tryRet(outInt(out, ar));
  return encodeTxt(out, sx, uniStrLen(sx));
}

retCode encodeCons(ioPo out, integer arity){
  tryRet(outChar(out,dtaTrm));
  return outInt(out,arity);
}
