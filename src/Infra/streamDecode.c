//
// Created by Francis McCabe on 2019-03-17.
//

#include "streamDecode.h"
#include <hash.h>
#include <unistd.h>
#include <base64.h>
#include "verifyP.h"
#include "manifest.h"
#include "streamDecodeP.h"
#include "signature.h"
#include "multiP.h"

static logical isDigit(codePoint ch) {
  return (logical) (ch >= '0' && ch <= '9');
}

static int digitVal(codePoint ch) {
  return (int) (ch - '0');
}

retCode decInt(ioPo in, integer *ii) {
  codePoint ch;
  integer result = 0;

  switch (inChar(in, &ch)) {
    case Ok:
      if (ch == '-') {
        retCode ret = decInt(in, ii);
        *ii = -(*ii);
        return ret;
      } else if (isDigit(ch)) {
        result = digitVal(ch); // First digit of number

        while (True) {
          byte chb;
          switch (inByte(in, &chb)) {
            case Ok:
              if (isDigit(chb)) {
                result = result * 10 + digitVal(chb);
                continue;
              } else {
                *ii = result;
                return putBackByte(in, chb);
              }
            case Eof: {
              *ii = result;
              return Ok;
            }
            default:
              return Eof;
          }
        }
      } else {
        return Eof;
      }
    case Eof:
    default:
      return Eof;
  }
}

retCode decodeInteger(ioPo in, integer *ix) {
  if (isLookingAt(in, "x") == Ok)
    return decInt(in, ix);
  else
    return Fail;
}

retCode decodeText(ioPo in, strBufferPo buffer) {
  codePoint delim;
  clearStrBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      if (ch == '\\')
        ret = inChar(in, &ch);
      if (ret == Ok)
        ret = outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}

retCode decodeString(ioPo in, char *buffer, integer buffLen) {
  if (isLookingAt(in, "s") == Ok) {
    strBufferPo b = fixedStringBuffer(buffer, buffLen);
    retCode ret = decodeText(in, b);
    if (ret == Ok)
      ret = outByte(O_IO(b), 0);
    closeIo(O_IO(b));
    return ret;
  } else
    return Fail;
}

retCode decodeName(ioPo in, char *buffer, integer buffLen, integer *length) {
  if (isLookingAt(in, "s") == Ok) {
    strBufferPo b = fixedStringBuffer(buffer, buffLen);
    retCode ret = decodeText(in, b);
    *length = strBufferLength(b);
    closeIo(O_IO(b));
    return ret;
  } else
    return Fail;
}

retCode decFlt(ioPo in, double *dx) {
  strBufferPo tmpBuffer = newStringBuffer();
  retCode ret = decodeText(in, tmpBuffer);

  if (ret == Ok) {
    integer len = 0;
    char *const s = getTextFromBuffer(tmpBuffer, &len);
    *dx = parseNumber(s, len);
  }

  closeIo(O_IO(tmpBuffer));
  return ret;
}

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, strBufferPo buff, char *errorMsg, integer msgLen);

retCode streamDecode(ioPo in, decodeCallBackPo cb, void *cl, char *errorMsg, integer msgLen) {
  strBufferPo strBuffer = newStringBuffer();
  retCode ret = cb->startDecoding(cl);

  if (ret == Ok)
    ret = decodeStream(in, cb, cl, strBuffer, errorMsg, msgLen);

  if (ret == Ok)
    cb->endDecoding(cl);

  closeIo(O_IO(strBuffer));
  return ret;
}

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, strBufferPo buff, char *errorMsg, integer msgLen) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case vodTrm: {
      return cb->decVoid(cl);
    }
    case intTrm: {
      integer i;
      res = decInt(in, &i);
      if (res == Ok)
        res = cb->decInt(i, cl);
      return res;
    }
    case fltTrm: {
      double dx;
      res = decFlt(in, &dx);
      if (res == Ok)
        res = cb->decFlt(dx, cl);
      return res;
    }
    case bigTrm: {
      clearStrBuffer(buff);
      res = decodeText(in, buff);

      if (res == Ok) {
        integer len;
        char *txt = getTextFromBuffer(buff, &len);
        uint32 data[len];
        integer count = longFromText(txt, len, data, len);
        res = cb->decBignum(data, count, cl);
      }
      return res;
    }
    case enuTrm: {
      clearStrBuffer(buff);

      res = decodeText(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(buff, &len);
        res = cb->decLbl(nm, 0, cl);
      }
      return res;
    }
    case lblTrm: {
      integer arity;
      clearStrBuffer(buff);

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = decodeText(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(buff, &len);
        res = cb->decLbl(nm, arity, cl);
      }
      return res;
    }
    case chrTrm: {
      codePoint cp;
      retCode ret = inChar(in, &cp);
      if (ret == Ok)
        ret = cb->decChar(cp, cl);
      return ret;
    }
    case strTrm: {
      clearStrBuffer(buff);
      res = decodeText(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(buff, &len);
        res = cb->decString(nm, len, cl);
      }
      return res;
    }
    case dtaTrm: {
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if (res == Ok)
        res = cb->decCons(arity, cl);

      if (res == Ok)
        res = decodeStream(in, cb, cl, buff, errorMsg, msgLen); // Handle the operator of the cons

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = decodeStream(in, cb, cl, buff, errorMsg, msgLen);

        if (res == Ok)
          res = cb->endCons(cl);
      }

      return res;
    }

    case lstTrm: {
      integer count;

      if ((res = decInt(in, &count)) != Ok) /* How many elements in the list */
        return res;

      if (res == Ok)
        res = cb->decLst(count, cl);

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < count; i++)
          res = decodeStream(in, cb, cl, buff, errorMsg, msgLen);

        if (res == Ok)
          res = cb->endLst(cl);
      }

      return res;
    }

    case cloTrm: {
      res = cb->decClo(cl);

      if (res == Ok)
        res = decodeStream(in, cb, cl, buff, errorMsg, msgLen); // Handle the label of the closure

      if (res == Ok) {
        res = decodeStream(in, cb, cl, buff, errorMsg, msgLen); // and the environment tuple

        if (res == Ok)
          res = cb->endClo(cl);
      }

      return res;
    }

    default:
      return Error;
  }

}

static retCode skipFlag(void *cl) {
  return Ok;
}

static retCode skipVoid(void *cl) {
  return Ok;
}

static retCode skipInt(integer ix, void *cl) {
  return Ok;
}

static retCode skipFlt(double dx, void *cl) {
  return Ok;
}

static retCode skipChar(codePoint cp, void *cl) {
  return Ok;
}

static retCode skipString(char *sx, integer len, void *cl) {
  return Ok;
}

static retCode skipName(char *sx, integer ar, void *cl) {
  return Ok;
}

static retCode skipRecLbl(char *sx, integer ar, FieldRec fields[], void *cl) {
  return Ok;
}

static retCode skipLst(integer arity, void *cl) {
  return Ok;
}

static retCode skipCns(integer ix, void *cl) {
  return Ok;
}

static retCode endSkipCns(void *cl) {
  return Ok;
}

static retCode endSkipLst(void *cl) {
  return Ok;
}

static retCode skipClosure(void *cl) {
  return Ok;
}

static retCode endSkipClosure(void *cl) {
  return Ok;
}

static retCode skipBignum(uint32 *data, integer count, void *cl) {
  return Ok;
}

static DecodeCallBacks skipCB = {
  skipFlag,           // startDecoding
  skipFlag,           // endDecoding
  skipVoid,           // decVoid
  skipInt,            // decInt
  skipFlt,            // decFlt
  skipName,           // decLbl
  skipRecLbl,         // record label
  skipChar,           // Character
  skipString,         // decString
  skipCns,            // decCons
  endSkipCns,         // End of constructor
  skipLst,            // Start of list
  endSkipLst,         // End of list
  skipClosure,        // Start the skipping of a closure
  endSkipClosure,        // End the skipping of a closure
  skipBignum,         // skip over a big number
};

retCode skipEncoded(ioPo in, char *errorMsg, long msgLen) {
  switch (streamDecode(in, &skipCB, NULL, errorMsg, msgLen)) {
    case Ok:
      return Ok;
    case Error:
      strMsg(errorMsg, msgLen, "problem in decoding");
      return Error;
    case Eof:
      strMsg(errorMsg, msgLen, "unexpected EOF");
      return Error;
    default:
      strMsg(errorMsg, msgLen, "problem in decoding");
      return Error;
  }
}

typedef struct {
  ioPo out;
} CopyRec;

static retCode copyFlag(void *cl) {
  return Ok;
}

static retCode copyVoid(void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return outChar(out, vodTrm);
}

static retCode copyInt(integer ix, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeInt(out, ix);
}

static retCode copyFlt(double dx, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeFlt(out, dx);
}

static retCode copyChar(codePoint cp, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeChar(out, cp);
}

static retCode encodeName(ioPo out, char *sx, integer len) {
  codePoint delim = uniSearchDelims(sx, len, (char *) ";\"|/%");
  if (delim == 0)
    delim = '"';

  retCode ret = outChar(out, delim);
  integer ix = 0;
  while (ret == Ok && ix < len) {
    codePoint ch = nextCodePoint(sx, &ix, len);
    if (ch == '\\')
      ret = outStr(out, "\\\\");
    else if (ch == delim) {
      ret = outChar(out, '\\');
      if (ret == Ok)
        ret = outChar(out, delim);
    } else
      ret = outChar(out, ch);
  }
  if (ret == Ok)
    ret = outChar(out, delim);
  return ret;
}

static retCode copyRecLbl(char *sx, integer ar, FieldRec fields[], void *cl) {
  return Error;
}

static retCode copyString(char *sx, integer len, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeStr(out, sx, len);
}

static retCode copyEnum(char *nm, integer ar, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeLbl(out, nm, ar);
}

static retCode copyCons(integer ar, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, dtaTrm);
  return encodeInt(out, ar);
}

static retCode endCopyCons(void *cl) {
  return Ok;
}

static retCode copyList(integer ar, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, lstTrm);
  return encodeInt(out, ar);
}

static retCode endCopyList(void *cl) {
  return Ok;
}

static retCode copyClosure(void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return outChar(out, cloTrm);
}

static retCode endCopyClosure(void *cl) {
  return Ok;
}

static retCode copyBignum(uint32 *data, integer count, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, bigTrm);
  integer buffLen = count * INT32_DIGITS + 1;
  char buffer[buffLen];

  integer textCount = textFromlong(buffer, buffLen, data, count);
  return encodeStr(out, buffer, textCount);
}

static DecodeCallBacks copyCB = {
  copyFlag,           // startDecoding
  copyFlag,           // endDecoding
  copyVoid,           // decVoid
  copyInt,            // decInt
  copyFlt,            // decFlt
  copyEnum,           // decLbl
  copyRecLbl,          // record label
  copyChar,           // character
  copyString,         // decString
  copyCons,            // decCons
  endCopyCons,        // Copying a structure
  copyList,           // Copy a list
  endCopyList,        // End of copying a list
  copyClosure,                // start copying a closure
  endCopyClosure,
  copyBignum,         // Copy a big number
};

retCode decodeLbl(ioPo in, char *nm, long nmLen, integer *arity,
                  char *errorMsg, integer msgLen) {
  if (isLookingAt(in, "o") == Ok) {
    retCode ret = decInt(O_IO(in), arity);

    if (ret != Ok)
      return ret;
    else {
      strBufferPo pkgB = fixedStringBuffer(nm, nmLen);
      ret = decodeText(O_IO(in), pkgB);
      outByte(O_IO(pkgB), 0);
      closeIo(O_IO(pkgB));
      return ret;
    }
  } else if (isLookingAt(in, "e") == Ok) {
    strBufferPo pkgB = fixedStringBuffer(nm, nmLen);
    retCode ret = decodeText(O_IO(in), pkgB);
    outByte(O_IO(pkgB), 0);
    closeIo(O_IO(pkgB));
    *arity = 0;
    return ret;
  } else {
    strMsg(errorMsg, msgLen, "invalid label encoding");
    return Error;
  }
}
