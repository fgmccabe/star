
#include <unistd.h>
#include <hash.h>
#include "decodeP.h"
#include <globals.h>
#include <array.h>
#include <arrayP.h>
#include <decodeP.h>
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
                      res = reserveSpace(H, amnt);

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

static retCode estimateVoid(void *cl) {
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

static retCode endEstimateCns(void *cl) {
  return Ok;
}

static retCode estimateLst(integer count, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += ListCellCount + BaseCellCount(count);
  return Ok;
}

static retCode endEstimateLst(void *cl) {
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
    estimateVoid,           // decVoid
    estimateInt,            // decInt
    estimateFlt,            // decFlt
    estimateLbl,           // decLbl
    estimateString,         // decString
    estimateCns,            // decCon
    endEstimateCns,         // End of constructor
    estimateLst,            // Start of list
    endEstimateLst          // End of list
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
    bufferPo b = fixedStringBuffer(buffer, buffLen);
    retCode ret = decodeText(in, b);
    outChar(O_IO(b), 0);
    closeFile(O_IO(b));
    return ret;
  } else
    return Fail;
}

retCode decFlt(ioPo in, double *dx) {
  bufferPo tmpBuffer = newStringBuffer();
  retCode ret = decodeText(in, tmpBuffer);

  if (ret == Ok) {
    integer len = 0;
    char *const s = getTextFromBuffer(tmpBuffer, &len);
    *dx = parseNumber(s, len);
  }

  closeFile(O_IO(tmpBuffer));
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
    case enuTrm: {
      clearBuffer(buff);

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
      clearBuffer(buff);

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
    case strTrm: {
      clearBuffer(buff);
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
        res = decodeStream(in, cb, cl, buff); // Handle the operator of the cons

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = decodeStream(in, cb, cl, buff);

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
          res = decodeStream(in, cb, cl, buff);

        if (res == Ok)
          res = cb->endLst(cl);
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

static retCode skipString(char *sx, integer len, void *cl) {
  return Ok;
}

static retCode skipName(char *sx, integer ar, void *cl) {
  return Ok;
}

static retCode skipLst(integer ix, void *cl) {
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

static DecodeCallBacks skipCB = {
  skipFlag,           // startDecoding
  skipFlag,           // endDecoding
  skipVoid,           // decVoid
  skipInt,            // decInt
  skipFlt,            // decFlt
  skipName,           // decLbl
  skipString,         // decString
  skipCns,            // decCons
  endSkipCns,         // End of constructor
  skipLst,            // Start of list
  endSkipLst          // End of list
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

static retCode copyVoid(void *cl) {
  ioPo out = ((CopyRec *) cl)->out;
  return outChar(out, vodTrm);
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

static DecodeCallBacks copyCB = {
  copyFlag,           // startDecoding
  copyFlag,           // endDecoding
  copyVoid,           // decVoid
  copyInt,            // decInt
  copyFlt,            // decFlt
  copyEnum,           // decLbl
  copyString,         // decString
  copyCons,            // decCons
  endCopyCons,        // Copying a structure
  copyList,           // Copy a list
  endCopyList         // End of copying a list
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
    case vodTrm: {
      *tgt = (termPo) voidEnum;
      return Ok;
    }
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
    case enuTrm: {
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        *tgt = (termPo) declareLbl(getTextFromBuffer(tmpBuffer, &len), 0);
      }
      return res;
    }
    case lblTrm: {
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        *tgt = (termPo) declareLbl(getTextFromBuffer(tmpBuffer, &len), arity);
      }
      return res;
    }
    case strTrm: {
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        const char *txt = getTextFromBuffer(tmpBuffer, &len);
        *tgt = (termPo) allocateString(H, txt, len);
      }
      return res;
    }
    case dtaTrm: {
      termPo lbl;
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decode(in, S, H, &lbl, tmpBuffer)) != Ok)
        return res;

      if (res == Ok) {
        if (labelArity(C_LBL(lbl)) != arity)
          res = Error;
      }

      if (res == Ok) {
        int root = gcAddRoot(H, &lbl);
        normalPo obj = allocateStruct(H, C_LBL(lbl));
        *tgt = (termPo) (obj);

        termPo el = voidEnum;
        gcAddRoot(H, &el);

        // In case of GC, we mark all the elements as void before doing any decoding
        for (integer ix = 0; ix < arity; ix++)
          setArg(obj, ix, voidEnum);

        for (integer i = 0; res == Ok && i < arity; i++) {
          res = decode(in, S, H, &el, tmpBuffer); /* read each element of term */
          if (res == Ok)
            setArg(obj, i, el);
        }

        gcReleaseRoot(H, root);
      }

      return res;
    }

    case lstTrm: {
      termPo lbl;
      integer count;

      if ((res = decInt(in, &count)) != Ok) /* How many elements in the list */
        return res;

      if (res == Ok) {
        int root = gcAddRoot(H, &lbl);
        listPo lst = allocateList(H, count, True);
        *tgt = (termPo) (lst);

        termPo el = voidEnum;
        gcAddRoot(H, &el);

        for (integer i = 0; res == Ok && i < count; i++) {
          res = decode(in, S, H, &el, tmpBuffer); /* read each element of term */
          if (res == Ok)
            lst = appendToList(H, lst, el);
        }

        gcReleaseRoot(H, root);
      }

      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}
