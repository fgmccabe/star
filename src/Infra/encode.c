//
// Created by Francis McCabe on 1/13/18.
//
#include "config.h"
#include <ooio.h>
#include <star.h>
#include "encoding.h"
#include "signature.h"

retCode encodeInt(ioPo out, int64 ix) {
  tryRet(outChar(out, intTrm));
  return outInt(out, ix);
}

retCode encodeChar(ioPo out, codePoint cp) {
  tryRet(outChar(out, chrTrm));
  return outChar(out, cp);
}

retCode encodeStr(ioPo out, char *str, integer len) {
  tryRet(outChar(out, strTrm));
  return encodeTxt(out, str, len);
}

codePoint findDelim(char *str, integer srcLen, char *choices, codePoint deflt) {
  integer chPos = 0;
  integer choiceLen = uniStrLen(choices);

  while (chPos < choiceLen) {
    codePoint ch = nextCodePoint(choices, &chPos, choiceLen);
    int64 pos = uniIndexOf(str, srcLen, 0, ch);

    if (pos < 0)
      return ch; // We found our delimiter
  }
  return deflt;
}

retCode encodeTxt(ioPo out, char *str, integer len) {
  codePoint delim = findDelim(str, len, "'\"|/%", '\"');
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

retCode encodeEnum(ioPo out, char *nm) {
  tryRet(outChar(out, enuTrm));
  return encodeTxt(out, nm, uniStrLen(nm));
}

retCode encodeFlt(ioPo out, double dx) {
  tryRet(outChar(out, fltTrm));
  return outFloat(out, dx);
}

retCode encodeLbl(ioPo out, char *sx, integer ar) {
  tryRet(outChar(out, lblTrm));
  tryRet(outInt(out, ar));
  return encodeTxt(out, sx, uniStrLen(sx));
}

retCode encodeTplLbl(ioPo out, integer ar) {
  tryRet(outChar(out, lblTrm));
  tryRet(outInt(out, ar));
  char txt[MAX_SYMB_LEN];

  strMsg(txt, NumberOf(txt), "()%d", ar);

  return encodeTxt(out, txt, uniStrLen(txt));
}

retCode encodeLst(ioPo out, integer ar) {
  tryRet(outChar(out, lstTrm));
  return outInt(out, ar);
}

retCode encodeCons(ioPo out, integer arity) {
  tryRet(outChar(out, dtaTrm));
  return outInt(out, arity);
}

retCode encodePkgName(ioPo out, packagePo pkg) {
  char *name = pkgName(pkg);
  char *version = pkgVers(pkg);

  tryRet(encodeCons(out, 2));
  tryRet(encodeLbl(out, "pkg", 2));

  tryRet(encodeStr(out, name, uniStrLen(name)));

  if (uniIsLit(version, "*")) {
    return encodeEnum(out, "*");
  } else {
    tryRet(encodeLbl(out, "v", 1));
    return encodeStr(out, version, uniStrLen(version));
  }
}
