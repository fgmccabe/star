
#include <unistd.h>
#include <hash.h>
#include "decodeP.h"
#include <globals.h>
#include <cons.h>
#include <consP.h>
#include "arithP.h"
#include "strP.h"
#include "heap.h"
#include "signature.h"
#include "labelsP.h"
#include "streamDecode.h"
#include "debug.h"

#ifdef TRACEDECODE
tracingLevel traceDecode = noTracing;
#endif

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

static retCode estimateRecLbl(char *nm, integer arity, FieldRec fields[], void *cl) {
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

  info->amnt += count * CONS_CELLCOUNT;
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
    estimateRecLbl,         // record label
    estimateString,         // decString
    estimateCns,            // decCon
    endEstimateCns,         // End of constructor
    estimateLst,            // Start of list
    endEstimateLst          // End of list
  };

  retCode ret = streamDecode(in, &estimateCB, &info, NULL, 0);

  if (ret == Ok) {
    *amnt = info.amnt;
  }

  return ret;
}

/*
 Warning: caller assumes responsibility for ensuring that tgt is a valid root
 */

static retCode decodeList(ioPo in, encodePo S, integer count, heapPo H, termPo *tgt, bufferPo tmpBuffer);

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, bufferPo tmpBuffer) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch ((starDecodeKey) ch) {
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
        *tgt = (termPo) declareEnum(getTextFromBuffer(tmpBuffer, &len), H);
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
    case recLbl: {
      return decodeRecLbl(in, S, tgt, tmpBuffer, S->errorMsg, S->msgSize);
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
        if (labelArity(C_LBL(lbl)) != arity) {
          strMsg(S->errorMsg, S->msgSize, "invalid label arity: expecting %d", arity);
          res = Error;
        }
      }

      if (res == Ok) {
        int root = gcAddRoot(H, &lbl);
        normalPo obj = allocateStruct(H, C_LBL(lbl));
        *tgt = (termPo) (obj);

        termPo el = voidEnum;
        gcAddRoot(H, &el);
        gcAddRoot(H, (ptrPo) &obj);

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
      integer count;

      if ((res = decInt(in, &count)) != Ok) /* How many elements in the list */
        return res;

      if (res == Ok)
        res = decodeList(in, S, count, H, tgt, tmpBuffer);

      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

retCode decodeList(ioPo in, encodePo S, integer count, heapPo H, termPo *tgt, bufferPo tmpBuffer) {
  if (count == 0) {
    *tgt = (termPo) nilEnum;
    return Ok;
  } else {
    retCode res = decodeList(in, S, count - 1, H, tgt, tmpBuffer);

    if (res == Ok) {
      termPo el;
      int root = gcAddRoot(H, &el);
      res = decode(in, S, H, &el, tmpBuffer); /* read each element of term */
      if (res == Ok)
        *tgt = (termPo) allocateCons(H, el, *tgt);
      gcReleaseRoot(H, root);
    }
    return res;
  }
}

retCode decodeTplCount(ioPo in, integer *count, char *errorMsg, integer msgSize) {
  if (isLookingAt(in, "n") == Ok) {
    char nm[MAXLINE];
    integer ar;
    retCode ret = decInt(in, count);
    if (ret == Ok) {
      ret = decodeLbl(in, nm, NumberOf(nm), &ar, errorMsg, msgSize);
      if (ret == Ok) {
        if (ar != *count) {
          strMsg(errorMsg, msgSize, "invalid tuple arity encoding");
          return Error;
        }
      }
    }
    return ret;
  } else
    return Fail;
}

static retCode
decodeRecordFieldTypes(ioPo in, encodePo S, integer count, fieldTblPo *tgt, bufferPo tmpBuffer, char *errorMsg,
                       integer msgSize);

retCode decodeRecLbl(ioPo in, encodePo S, termPo *tgt, bufferPo tmpBuffer, char *errorMsg, integer msgSize) {
  char lblName[MAX_SYMB_LEN];
  bufferPo pkgB = fixedStringBuffer(lblName, NumberOf(lblName));

  retCode ret = decodeText(in, pkgB);

  if (ret == Ok) {
    integer lblLen;
    char *Nm = getTextFromBuffer(pkgB, &lblLen);

#ifdef TRACEDECODE
    if (traceDecode >= detailedTracing)
      outMsg(logFile, "decoding record label %s\n%_", lblName);
#endif

    if ((ret = isLookingAt(in, "{")) == Ok) {
      fieldTblPo tbl = Null;

      ret = decodeRecordFieldTypes(in, S, 0, &tbl, tmpBuffer, errorMsg, msgSize);

      if (ret == Ok && tbl != Null) {
        labelPo lbl = declareLbl(Nm, tbl->size);
        declareFields(lbl, tbl);

        *tgt = (termPo) lbl;
      }
    }
  }
  closeFile(O_IO(pkgB));

//  outMsg(logFile, "%#T\n", *tgt);
  return ret;
}

retCode
decodeRecordFieldTypes(ioPo in, encodePo S, integer count, fieldTblPo *tgt, bufferPo tmpBuffer, char *errorMsg,
                       integer msgSize) {
  if (isLookingAt(in, "}") == Ok) {
    *tgt = newFieldTable(count);
#ifdef TRACEDECODE
    if (traceDecode >= detailedTracing)
      outMsg(logFile, "%d fields in record\n%_", count);
#endif
    return Ok;
  } else {
    char fieldName[MAX_SYMB_LEN];

    bufferPo pkgB = fixedStringBuffer(fieldName, NumberOf(fieldName));
    retCode ret = decodeText(O_IO(in), pkgB);
    outByte(O_IO(pkgB), 0);
    closeFile(O_IO(pkgB));

    if (ret == Ok) {
      labelPo field = declareLbl(fieldName, 0);

#ifdef TRACEDECODE
      if (traceDecode >= detailedTracing)
        outMsg(logFile, "record field %s\n%_", fieldName);
#endif

      ret = skipSignature(in); // Field signature
      if (ret == Ok) {
        ret = decodeRecordFieldTypes(in, S, count + 1, tgt, tmpBuffer, errorMsg, msgSize);
        if (ret == Ok)
          setFieldTblEntry(*tgt, field, count);
      }
    }
    return ret;
  }
}
