
#include <unistd.h>
#include <hash.h>
#include "decodeP.h"
#include <globals.h>
#include "arithP.h"
#include "strP.h"
#include "heap.h"
#include "signature.h"
#include "labels.h"

/*
 * Decode a structure from an input stream
 */

static retCode estimate(ioPo in, integer *amnt);

retCode decodeTerm(ioPo in, heapPo H, termPo *tgt, char *errorMsg, long msgSize) {
  EncodeSupport support = {errorMsg, msgSize, H};
  logical isBlocked = False;
  logical isAsynched = False;

  if (objectHasClass(O_OBJECT(in), fileClass)) {
    if (!isFileBlocking(O_FILE(in))) {
      isBlocked = True;
      configureIo(O_FILE(in), turnOnBlocking);
    }
    if (isFileAsynch(O_FILE(in))) {
      isAsynched = True;
      configureIo(O_FILE(in), disableAsynch);
    }
  }

  codePoint ch;

  again:
  switch (inChar(in, &ch)) {
    case Eof:
      return Eof;
    default:
      strMsg(errorMsg, msgSize, "stream prematurely ended");
      return Error;
    case Interrupt:
      goto again;
    case Ok: {
      if (ch != strTrm) {
        strMsg(errorMsg, msgSize, "invalid lead-in in code sequence");
        goto error_exit;
      } else {
        codePoint delim;

        switch (inChar(in, &delim)) {
          case Ok: {
            bufferPo buffer = newStringBuffer();
            while (True) {
              switch (inChar(in, &ch)) {
                case Ok:
                  if (ch == delim) {
                    rewindBuffer(buffer);

                    integer amnt;

                    retCode res = estimate(O_IO(buffer), &amnt);

                    //logMsg(logFile,"Estimate of space requirements: %d heap, %d permanent",perm);


                    if (res == Ok)
                      res = reserveSpace((size_t) amnt);

                    if (res == Ok) {
                      rewindBuffer(buffer); /* re-read from string buffer */
                      bufferPo tmpBuffer = newStringBuffer();

                      res = decode(O_IO(buffer), &support, H, tgt, tmpBuffer);

                      closeFile(O_IO(buffer));
                      closeFile(O_IO(tmpBuffer));
                    }

                    if (objectHasClass(O_OBJECT(in), fileClass)) {
                      if (isBlocked)
                        configureIo(O_FILE(in), turnOffBlocking);
                      if (isAsynched)
                        configureIo(O_FILE(in), enableAsynch);
                    }
                    return res;
                  } else {
                    outChar(O_IO(buffer), ch);
                    continue;
                  }
                case Eof:
                  strMsg(errorMsg, msgSize, "unexpected eof");
                  goto error_exit;
                case Error:
                default:
                  strMsg(errorMsg, msgSize, "stream prematurely ended");
                  goto error_exit;
              }
            }
          }
          default:
            strMsg(errorMsg, msgSize, "stream prematurely ended");
            goto error_exit;
        }
      }
    }
  }
  error_exit:
  if (objectHasClass(O_OBJECT(in), fileClass)) {
    if (isBlocked)
      configureIo(O_FILE(in), turnOffBlocking);
    if (isAsynched)
      configureIo(O_FILE(in), enableAsynch);
  }
  return Error;
}

/*
 Estimate amount of heap space needed
 */

typedef struct {
  integer amnt;
} Estimation;

static retCode nullEstimate(void *cl) {
  return Ok;
}

static retCode estimateInt(integer _, void *cl) {
  Estimation *info = (Estimation *) cl;
  info->amnt += IntegerCellCount;
  return Ok;
}

static retCode estimateFlt(double dx, void *cl) {
  Estimation *info = (Estimation *) cl;
  info->amnt += FloatCellCount;
  return Ok;
}

static retCode estimateString(char *nm, integer size, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += CellCount(sizeof(StringRecord) + (size + 1) * sizeof(char));
  return Ok;
}

static retCode estimateLbl(char *nm, integer arity, void *cl) {
  return Ok;
}

static retCode estimateCns(integer arity, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += arity + 1;
  return Ok;
}

/*
 Estimate amount of heap space needed
 */
retCode estimate(ioPo in, integer *amnt) {
  Estimation info = {0};

  DecodeCallBacks estimateCB = {
    nullEstimate,           // startDecoding
    nullEstimate,           // endDecoding
    estimateInt,            // decInt
    estimateFlt,            // decFlt
    estimateLbl,           // decLbl
    estimateString,         // decString
    estimateCns             // decCon
  };

  retCode ret = streamDecode(in, &estimateCB, &info);

  if (ret == Ok) {
    *amnt = info.amnt;
  }

  return ret;
}

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

retCode decodeName(ioPo in, bufferPo buffer) {
  codePoint delim;
  clearBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim) {
      outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}

retCode decodeNm(ioPo in, char *buffer, integer buffLen) {
  codePoint delim;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    integer ix = 0;
    while (ix < buffLen && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      ix = appendCodePoint(buffer, &ix, buffLen, ch);
    }
    if (ix < buffLen) {
      appendCodePoint(buffer, &ix, buffLen, 0);
      return ret;
    } else
      return Space;
  }
}

retCode decodeText(ioPo in, bufferPo buffer) {
  codePoint delim;
  clearBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      if (ch == '\\')
        ret = inChar(in, &ch);
      outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}

retCode decodeString(ioPo in, char *buffer, integer buffLen) {
  if (isLookingAt(in, "s") == Ok) {
    return decodeNm(in, buffer, buffLen);
  } else
    return Fail;
}

retCode decFlt(ioPo in, double *dx) {
  bufferPo tmpBuffer = newStringBuffer();
  retCode ret = decodeName(in, tmpBuffer);

  if (ret == Ok) {
    integer len;
    *dx = parseNumber(getTextFromBuffer(&len, tmpBuffer), len);
  }
  return ret;
}

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, bufferPo buff);

retCode streamDecode(ioPo in, decodeCallBackPo cb, void *cl) {
  bufferPo strBuffer = newStringBuffer();
  retCode ret = cb->startDecoding(cl);

  if (ret == Ok)
    ret = decodeStream(in, cb, cl, strBuffer);

  if (ret == Ok)
    cb->endDecoding(cl);

  closeFile(O_IO(strBuffer));
  return ret;
}

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, bufferPo buff) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
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

    case enuTrm: {
      clearBuffer(buff);

      res = decodeName(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(&len, buff);
        res = cb->decLbl(nm, 0, cl);
      }
      return res;
    }

    case lblTrm: {
      integer arity;
      clearBuffer(buff);

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = decodeName(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(&len, buff);
        res = cb->decLbl(nm, arity, cl);
      }
      return res;
    }

    case strTrm: {
      clearBuffer(buff);
      res = decodeText(in, buff);

      if (res == Ok) {
        integer len;
        char *nm = getTextFromBuffer(&len, buff);
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
        res = decodeStream(in, cb, cl, buff); // Handle the operator of the cons

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = decodeStream(in, cb, cl, buff);
      }

      return res;
    }

    default:
      return Error;
  }
}

retCode processTpl(ioPo in, heapPo heap, char *errorMsg, long msgSize, decodeFun dec, void *cl) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  else if (ch != dtaTrm) {
    strMsg(errorMsg, msgSize, "Not a tuple");
    return Error;
  } else {
    integer arity;

    if ((res = decInt(in, &arity)) != Ok) { /* How many arguments in the class */
      strMsg(errorMsg, msgSize, "Not a tuple");
      return res;
    } else if ((res = inChar(in, &ch)) == Ok && ch == lblTrm) {
      integer ar;

      if ((res = decInt(in, &ar)) != Ok)
        return res;
      bufferPo tmpBuffer = newStringBuffer();

      res = decodeName(in, tmpBuffer);

      closeFile(O_IO(tmpBuffer));
      for (integer ix = 0; res == Ok && ix < arity; ix++) {
        res = dec(in, heap, errorMsg, msgSize, ix, cl);
      }
    }
    return res;
  }
}

static retCode skipFlag(void *cl) {
  return Ok;
}

static retCode skipInt(integer ix, void *cl) {
  return Ok;
}

static retCode skipFlt(double dx, void *cl) {
  return Ok;
}

static retCode skipString(char *sx, integer len, void *cl) {
  return Ok;
}

static retCode skipName(char *sx, integer ar, void *cl) {
  return Ok;
}

static DecodeCallBacks skipCB = {
  skipFlag,           // startDecoding
  skipFlag,           // endDecoding
  skipInt,            // decInt
  skipFlt,            // decFlt
  skipName,           // decLbl
  skipString,         // decString
  skipInt             // decCons
};

retCode skipEncoded(ioPo in, char *errorMsg, long msgLen) {
  switch (streamDecode(in, &skipCB, NULL)) {
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

static retCode copyInt(integer ix, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, intTrm);
  return encodeInt(out, ix);
}

static retCode copyFlt(double dx, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, fltTrm);
  return encodeFlt(out, dx);
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

static retCode copyString(char *sx, integer len, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeStr(out, sx, len);
}

static retCode copyEnum(char *nm, integer ar, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return encodeStrct(out, nm, ar);
}

static retCode copyCons(integer ar, void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  outChar(out, dtaTrm);
  return encodeInt(out, ar);
}

static DecodeCallBacks copyCB = {
  copyFlag,           // startDecoding
  copyFlag,           // endDecoding
  copyInt,            // decInt
  copyFlt,            // decFlt
  copyEnum,           // decLbl
  copyString,         // decString
  copyCons            // decCons
};

retCode copyEncoded(ioPo in, ioPo out, char *errorMsg, long msgLen) {
  CopyRec rc = {out};

  switch (streamDecode(in, &copyCB, &rc)) {
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

/*
 Warning: caller assumes responsibility for ensuring that tgt is a valid root
 */


retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, bufferPo tmpBuffer) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case intTrm: {
      integer i;
      if ((res = decInt(in, &i)) != Ok)
        return res;
      *tgt = (termPo) allocateInteger(H, i);
      return Ok;
    }
    case fltTrm: {
      double dx;
      if ((res = decFlt(in, &dx)) != Ok)
        return res;
      *tgt = (termPo) allocateFloat(H, dx);
      return Ok;
    }
    case lblTrm: {
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeName(in, tmpBuffer)) == Ok) {
        integer len;
        *tgt = (termPo) declareLbl(getTextFromBuffer(&len, tmpBuffer), arity);
      }
      return res;
    }

    case dtaTrm: {
      termPo class;
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decode(in, S, H, &class, tmpBuffer)) != Ok)
        return res;

      if (res == Ok) {
        int root = gcAddRoot(H,&class);
        normalPo obj = allocateStruct(H, C_LBL(class));
        *tgt = (termPo) (obj);

        termPo el = voidEnum;
        gcAddRoot(H,&el);
        integer i;

        for (i = 0; i < arity; i++) {
          if ((res = decode(in, S, H, &el, tmpBuffer)) != Ok) /* read each element of term */
            break; /* we might need to skip out early */
          else {
            setArg(obj, i, el); /* stuff in the new element */
          }
        }
        gcReleaseRoot(H,root);
      }

      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}
