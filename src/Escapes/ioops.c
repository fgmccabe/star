//
// Created by Francis McCabe on 3/5/18.
//

#include <strings.h>
#include <assigns.h>
#include <arith.h>
#include <arithP.h>
#include <errorCodes.h>
#include <cons.h>
#include "future.h"
#include "ioops.h"
#include "globals.h"
#include "consP.h"
#include "charP.h"
#include "vectP.h"
#include "futureP.h"

static termPo ioErrorCode(retCode ret);

typedef retCode (*nextProc)(ioPo in, ioPo out, void *cl);

typedef struct {
  nextProc next;
  void *cl;
} AsyncStruct;

ReturnStatus g__close(heapPo h, termPo xc, termPo a1) {
  if (closeIo(ioChannel(C_IO(a1))) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__end_of_file(heapPo h, termPo a1) {
  termPo Rs = (isFileAtEof(ioChannel(C_IO(a1))) == Eof ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__inchar(heapPo h, termPo xc, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  codePoint cp;
  retCode ret = inChar(io, &cp);
  switch (ret) {
    case Ok:
      return (ReturnStatus) {.ret=Ok, .result=allocateCharacter(cp)};
    case Eof:
      return (ReturnStatus) {.ret=Error, .result=eofEnum};
    default:
      return (ReturnStatus) {.ret=Error, .result=eIOERROR};
  }
}

static retCode checkInChar(futurePo ft, heapPo h, void *cl, void *cl2) {
  filePo f = O_FILE(cl);
  switch (asyncStatus(f)) {
    case inProgress:
      return Fail;
    case completed: {
      codePoint cp;
      retCode ret = inChar(O_IO(f), &cp);

      if (ret == Ok) {
        return resolveFuture(ft, allocateCharacter(cp));
      } else
        return rejectFuture(ft, ioErrorCode(ret));
    }
    case canceled: {
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      return rejectFuture(ft, eIOERROR);
    }
  }
}

ReturnStatus g__inchar_async(heapPo h, termPo xc, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, checkInChar, io, Null);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else {
    codePoint cp;
    retCode ret = inChar(io, &cp);
    if (ret == Ok) {
      return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, allocateCharacter(cp), isAccepted)};
    } else {
      return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, ioErrorCode(ret), isRejected)};
    }
  }
}

ReturnStatus g__inchars(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer limit = integerVal(a2);

  strBufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (limit-- > 0 && ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    ReturnStatus rt = {.ret=Ok, .result=allocateString(h, text, length)};
    closeIo(O_IO(buffer));
    return rt;
  } else {
    closeIo(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__inbyte(heapPo h, termPo xc, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  byte b;
  retCode ret = inByte(io, &b);
  switch (ret) {
    case Ok:
      return (ReturnStatus) {.ret=Ok, .result=makeInteger(b)};
    case Eof:
      return (ReturnStatus) {.ret=Error, .result=eofEnum};
    default:
      return (ReturnStatus) {.ret=Error, .result=eIOERROR};
  }
}

termPo makeByte(heapPo h, integer ix, void *cl) {
  char *str = (char *) cl;
  integer ch = (byte) str[ix];
  return makeInteger(ch);
}

ReturnStatus g__inbytes(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer limit = integerVal(a2);

  strBufferPo buffer = newStringBuffer();
  byte bf[MAXLINE];

  retCode ret = Ok;

  while (limit-- > 0 && ret == Ok) {
    integer cnt;
    ret = inBytes(io, bf, NumberOf(bf), &cnt);

    if (ret == Ok)
      ret = outBytes(O_IO(buffer), bf, cnt, &cnt);
  }

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    termPo vect = makeVector(h, length, makeByte, (void *) text);

    return (ReturnStatus) {.ret=Ok, .result=vect};
  } else {
    closeIo(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=eIOERROR};
  }
}

ReturnStatus g__intext(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer mlen;
  const char *match = strVal(a2, &mlen);

  strBufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok) {
      if (uniIndexOf(match, mlen, 0, cp) >= 0) {
        break;
      } else
        ret = outChar(O_IO(buffer), cp);
    }
  }

  switch (ret) {
    case Ok: {
      integer length;
      char *text = getTextFromBuffer(buffer, &length);

      ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(h, text, length)};
      closeIo(O_IO(buffer));
      return rt;
    }
    case Eof: {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Eof, .result=eofEnum};
    }
    default: {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Error, .result=eIOERROR};
    }
  }
}

static retCode grabLine(ioPo io, strBufferPo buffer) {
  const char *match = "\n\r";
  integer mlen = uniStrLen(match);

  retCode ret = Ok;

  while (ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok) {
      if (uniIndexOf(match, mlen, 0, cp) >= 0) {
        break;
      } else
        ret = outChar(O_IO(buffer), cp);
    }
  }
  return ret;
}

ReturnStatus g__inline(heapPo h, termPo xc, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  strBufferPo buffer = newStringBuffer();

  retCode ret = grabLine(io, buffer);

  switch (ret) {
    case Ok: {
      integer length;
      char *text = getTextFromBuffer(buffer, &length);

      ReturnStatus rt = {.ret=Ok, .result=allocateString(h, text, length)};
      closeIo(O_IO(buffer));
      return rt;
    }
    case Eof: {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Eof, .result=eofEnum};
    }
    default: {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Error, .result=eIOERROR};
    }
  }
}

static retCode grabLineAsync(ioPo io, strBufferPo buffer) {
  const char *match = "\n\r";
  integer mlen = uniStrLen(match);

  retCode ret = Ok;
  while (ret == Ok) {
    switch ((ret = isInputReady(io, 1))) {
      case Ok: {
        codePoint cp;
        ret = inChar(io, &cp);
        if (ret == Ok) {
          if (uniIndexOf(match, mlen, 0, cp) >= 0) {
            return Ok;
          } else {
            ret = outChar(O_IO(buffer), cp);
            continue;
          }
        }
      }
      case Fail: {
        // We have to re-enqueue the input
        enqueueRead(O_FILE(io), Null, Null);
        return Fail;
      }
      default:
        return ret;
    }
  }
  return ret;
}

static retCode checkInLine(futurePo ft, heapPo h, void *cl, void *cl2) {
  filePo f = O_FILE(cl);
  strBufferPo buffer = O_BUFFER(cl2);
  switch (asyncStatus(f)) {
    case inProgress:
      return Fail;
    case completed: {
      retCode ret = grabLineAsync(O_IO(f), buffer);

      switch (ret) {
        case Ok: {
          int root = gcAddRoot(h, (ptrPo) &ft);

          termPo line = allocateFromStrBuffer(h, buffer);
          gcReleaseRoot(h, root);

          closeIo(O_IO(buffer));
          return resolveFuture(ft, line);
        }
        default:
          closeIo(O_IO(buffer));
          return rejectFuture(ft, ioErrorCode(ret));
        case Fail:
          return Ok;
      }
    }
    case canceled: {
      closeIo(O_IO(buffer));
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      closeIo(O_IO(buffer));
      return rejectFuture(ft, eIOERROR);
    }
  }
}

ReturnStatus g__inline_async(heapPo h, termPo xc, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  strBufferPo buffer = newStringBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, checkInLine, f, buffer);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else {
    retCode ret = grabLine(io, buffer);

    if (ret == Ok) {
      termPo line = allocateFromStrBuffer(h, buffer);
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, line, isAccepted)};
    } else {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, ioErrorCode(ret), isRejected)};
    }
  }
}

static retCode grabText(ioPo in, ioPo out) {
  retCode ret = Ok;
  while (ret == Ok) {
    codePoint cp;
    ret = inChar(in, &cp);
    if (ret == Ok)
      ret = outChar(out, cp);
  }

  return ret;
}

ReturnStatus g__get_file(heapPo h, termPo xc, termPo a1) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(a1), fn, NumberOf(fn));

  ioPo io = openInFile(fn, utf8Encoding);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();

    retCode ret = grabText(io, O_IO(buffer));

    if (ret == Eof) {
      termPo text = allocateFromStrBuffer(h, buffer);
      closeIo(O_IO(buffer));

      ReturnStatus rt = {.ret=Ok, .result=text};
      return rt;
    } else {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=ret, .result=voidEnum};
    }
  } else {
    return (ReturnStatus) {.ret=Error, .result=eNOTFND};
  }
}

static retCode grabTextAsync(ioPo io, strBufferPo buffer) {
  retCode ret = Ok;
  while (ret == Ok) {
    switch ((ret = isInputReady(io, 1))) {
      case Ok: {
        codePoint cp;
        ret = inChar(io, &cp);
        if (ret == Ok) {
          ret = outChar(O_IO(buffer), cp);
          continue;
        }
      }
      case Fail: {
        // We have to re-enqueue the input
        enqueueRead(O_FILE(io), Null, Null);
        return Fail;
      }
      case Eof:
        return Ok;
      default:
        return ret;
    }
  }
  return ret;
}

static retCode checkFileText(futurePo ft, heapPo h, void *cl, void *cl2) {
  filePo f = O_FILE(cl);
  strBufferPo buffer = O_BUFFER(cl2);
  switch (asyncStatus(f)) {
    case inProgress:
      return Fail;
    case completed: {
      retCode ret = grabTextAsync(O_IO(f), buffer);

      switch (ret) {
        case Ok: {
          int root = gcAddRoot(h, (ptrPo) &ft);

          termPo line = allocateFromStrBuffer(h, buffer);
          gcReleaseRoot(h, root);

          closeIo(O_IO(buffer));
          return resolveFuture(ft, line);
        }
        default:
          closeIo(O_IO(buffer));
          return rejectFuture(ft, ioErrorCode(ret));
        case Fail:
          return Ok;
      }
    }
    case canceled: {
      closeIo(O_IO(buffer));
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      closeIo(O_IO(buffer));
      return rejectFuture(ft, eIOERROR);
    }
  }
}

ReturnStatus g__getfile_async(heapPo h, termPo xc, termPo a1) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(a1), fn, NumberOf(fn));

  ioPo io = openInFile(fn, utf8Encoding);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();
    if (isAFile(O_OBJECT(io))) {
      filePo f = O_FILE(io);

      retCode ret = enableASynch(f);

      if (ret == Ok) {
        futurePo ft = makeFuture(h, voidEnum, checkFileText, f, newStringBuffer());

        if ((ret = enqueueRead(f, Null, Null)) == Ok)
          return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
      }
      return (ReturnStatus) {.ret=ret, .result=eNOPERM};
    } else {
      retCode ret = grabText(io, O_IO(buffer));

      if (ret == Eof) {
        termPo line = allocateFromStrBuffer(h, buffer);
        closeIo(O_IO(buffer));
        closeIo(io);
        return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, line, isAccepted)};
      } else {
        closeIo(O_IO(buffer));
        closeIo(io);
        return (ReturnStatus) {.ret=Ok, .result=(termPo) makeResolvedFuture(h, ioErrorCode(ret), isRejected)};
      }
    }
  } else
    return (ReturnStatus) {.ret=Error, .result=eNOTFND};
}

ReturnStatus g__outchar(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  codePoint cp = (codePoint) integerVal(a2);

  if (outChar(io, cp) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__outbyte(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer cp = integerVal(a2);

  if (outByte(io, cp) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__outbytes(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  retCode ret = Ok;

  while (ret == Ok && isCons(a2)) {
    byte b = (byte) integerVal(consHead(C_NORMAL(a2)));
    ret = outByte(io, b);
    a2 = consTail(C_NORMAL(a2));
  }

  if (ret == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__outtext(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer length;
  const char *text = strVal(a2, &length);
  retCode ret = Ok;

  integer pos = 0;

  while (ret == Ok && pos < length) {
    integer actual = length;
    ret = outBytes(io, (byte *) &text[pos], length, &actual);
    pos += actual;
  }

  if (ret == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__show(heapPo h, termPo a1) {
  integer length;
  const char *text = strVal(a1, &length);
  outMsg(logFile, "%S\n%_", text, length);
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

ReturnStatus g__put_file(heapPo h, termPo a1, termPo a2) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(a1), fn, NumberOf(fn));

  ioPo io = openOutFile(fn, utf8Encoding);
  if (io != Null) {
    integer tLen;
    const char *txt = strVal(a2, &tLen);

    retCode ret = outText(io, txt, tLen);
    closeIo(O_IO(io));

    ReturnStatus rt = {.ret=ret, .result=unitEnum};
    return rt;
  } else {
    return (ReturnStatus) {.ret=Error, .result=eNOTFND};
  }
}

ReturnStatus g__logmsg(heapPo h, termPo a1) {
  integer length;
  const char *text = strVal(a1, &length);

  if (logMsg(logFile, "%S", (char *) text, length) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Fail, .result=eIOERROR};
}

ReturnStatus g__display_depth(heapPo h) {
  return (ReturnStatus) {.ret=Ok, .result=makeInteger(displayDepth)};
}

ReturnStatus g__stdfile(heapPo h, termPo a1) {
  integer fNo = integerVal(a1);

  ReturnStatus rt = {.ret=Ok};

  switch (fNo) {
    case 0:
      rt.result = (termPo) stdInChnl(h);
      return rt;
    case 1:
      rt.result = (termPo) stdOutChnl(h);
      return rt;
    case 2:
    default:
      rt.result = (termPo) stdErrChnl(h);
      return rt;
  }
}

ReturnStatus g__fposition(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));
  integer pos = 0;

  switch (fileMode(io)) {
    case ioNULL:
      break;
    case ioREAD:
      pos = inCPos(io);
      break;
    case ioWRITE:
      pos = outBPos(io);
      break;
  }

  ReturnStatus rt = {.ret=Ok, .result=makeInteger(pos)};
  return rt;
}

ReturnStatus g__fseek(heapPo h, termPo xc, termPo a1, termPo a2) {
  filePo io = O_FILE(ioChannel(C_IO(a1)));
  integer pos = integerVal(a2);

  if (fileSeek(io, pos) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__setfileencoding(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer enc = integerVal(a2);
  setEncoding(io, (ioEncoding) enc);
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

ReturnStatus g__flush(heapPo h, termPo xc, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));
  if (isAFile(O_OBJECT(io)) && flushFile(O_FILE(io)) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__flushall(heapPo h, termPo a1) {
  flushOut();
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

termPo ioErrorCode(retCode ret) {
  switch (ret) {
    case Ok:
      return voidEnum;
    case Error:
      return eIOERROR;
    case Eof:
      return eofEnum;
    default:
      return eIOERROR;
  }
}
