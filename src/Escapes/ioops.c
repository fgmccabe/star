//
// Created by Francis McCabe on 3/5/18.
//

#include <strings.h>
#include <assigns.h>
#include <arith.h>
#include <arithP.h>
#include "errorCodes.h"
#include "future.h"
#include "ioops.h"
#include "globals.h"
#include "charP.h"
#include "vectP.h"
#include "futureP.h"
#include "byteBuffer.h"

static poolPo asyncPool = Null;

static retCode pollInput(futurePo ft, heapPo h, void *cl, void *cl2);

static taskState pushAsync(ioPo io, AsyncStruct *async);

static retCode lineCleanup(asyncPo async, retCode ret);

void initIoOps() {
  if (asyncPool == Null) {
    asyncPool = newPool(sizeof(AsyncStruct), 16);
  }
}

asyncPo
newAsyncTask(nextProc next, asyncAlloc alloc, asyncClose close, asyncCleanup cleanup, integer data, ioPo buffer) {
  asyncPo async = (asyncPo) allocPool(asyncPool);
  async->next = next;
  async->buffer = buffer;
  async->data = data;
  async->close = close;
  async->alloc = alloc;
  async->cleanup = cleanup;
  async->ret = Ok;
  async->state = notStarted;
  return async;
}

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

void asyncCloser(ioPo io, asyncPo async) {
  freePool(asyncPool, async);
}

static taskState oneChar(ioPo in, asyncPo async) {
  codePoint cp;
  retCode ret = inChar(in, &cp);
  if (ret == Ok) {
    async->data = cp;
    return succeeded;
  } else {
    async->ret = ret;
    return failure;
  }
}

static termPo allocChar(heapPo h, asyncPo sync) {
  codePoint cp = (codePoint) (sync->data);
  return allocateCharacter(cp);
}

static retCode oneCleanup(asyncPo sync, retCode ret) {
  return Eof;
}

ReturnStatus g__inchar_async(heapPo h, termPo xc, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(oneChar, allocChar, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

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

ReturnStatus g__inchars(heapPo h, termPo xc, termPo a1, termPo a2) {
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
    ReturnStatus rt = {.ret=Ok, .result=allocateFromStrBuffer(h, buffer)};
    closeIo(O_IO(buffer));
    return rt;
  } else {
    closeIo(O_IO(buffer));
    return (ReturnStatus) {.ret=Error, .result=ioErrorCode(ret)};
  }
}

static taskState nextChars(ioPo in, asyncPo async) {
  if (async->data > 0) {
    codePoint cp;
    retCode ret = inChar(in, &cp);
    if (ret == Ok) {
      ret = outChar(async->buffer, cp);
      async->data--;
      if (async->data == 0)
        return succeeded;
      else
        return running;
    } else {
      async->ret = ret;
      return failure;
    }
  } else
    return succeeded;
}

static void asyncStrCloser(ioPo io, asyncPo async) {
  closeIo(async->buffer);
  freePool(asyncPool, async);
}

static termPo allocStr(heapPo h, asyncPo async) {
  return allocateFromStrBuffer(h, O_BUFFER(async->buffer));
}

ReturnStatus g__inchars_async(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  integer limit = integerVal(a2);

  ioPo io = ioChannel(chnl);
  strBufferPo buffer = newStringBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextChars, allocStr, asyncStrCloser, lineCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else {
    retCode ret = Ok;
    while (limit-- > 0 && ret == Ok) {
      codePoint cp;
      ret = inChar(io, &cp);
      if (ret == Ok)
        ret = outChar(O_IO(buffer), cp);
    }

    if (ret == Ok) {
      ReturnStatus rt = {.ret=Ok, .result=allocateFromStrBuffer(h, buffer)};
      closeIo(O_IO(buffer));
      return rt;
    } else {
      closeIo(O_IO(buffer));
      return (ReturnStatus) {.ret=Error, .result=ioErrorCode(ret)};
    }
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

static termPo allocByte(heapPo h, asyncPo sync) {
  codePoint cp = (codePoint) (sync->data);
  return makeInteger((integer) cp);
}

static taskState oneByte(ioPo in, asyncPo async) {
  byte b;
  retCode ret = inByte(in, &b);
  if (ret == Ok) {
    async->data = b;
    return succeeded;
  } else {
    async->ret = ret;
    return failure;
  }
}

ReturnStatus g__inbyte_async(heapPo h, termPo xc, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);
    asyncPo async = newAsyncTask(oneByte, allocByte, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else {
    byte b;
    retCode ret = inByte(io, &b);
    switch (ret) {
      case Ok:
        return (ReturnStatus) {.ret=Ok, .result=makeInteger(b)};
      case Eof:
        return (ReturnStatus) {.ret=Error, .result=eofEnum};
      default:
        return (ReturnStatus) {.ret=Error, .result=ioErrorCode(ret)};
    }
  }
}

termPo makeByte(heapPo h, integer ix, void *cl) {
  char *str = (char *) cl;
  integer ch = (byte) str[ix];
  return makeInteger(ch);
}

ReturnStatus g__inbytes(heapPo h, termPo xc, termPo a1, termPo a2) {
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
    return (ReturnStatus) {.ret=Error, .result=ioErrorCode(ret)};
  }
}

static taskState nextBytes(ioPo in, asyncPo async) {
  if (async->data > 0) {
    byte by;
    retCode ret = inByte(in, &by);
    if (ret == Ok) {
      ret = appendByteToBuffer(O_BYTEBUFFER(async->buffer), by);
      async->data--;
      if (async->data == 0)
        return succeeded;
      else
        return running;
    } else {
      async->ret = ret;
      return failure;
    }
  } else
    return succeeded;
}

static termPo allocBytes(heapPo h, asyncPo async) {
  integer length;
  byte *bts = getBytesFromBuffer(O_BYTEBUFFER(async->buffer), &length);

  return makeVector(h, length, makeByte, (void *) bts);
}

static retCode bytesCleanup(asyncPo async, retCode ret) {
  switch (ret) {
    case Eof:
      if (byteBufferLength(O_BYTEBUFFER(async->buffer)) > 0)
        return Ok;
      else
        return Eof;
    default:
      return ret;
  }
}

ReturnStatus g__inbytes_async(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  integer limit = integerVal(a2);

  ioPo io = ioChannel(chnl);
  byteBufferPo buffer = newByteBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextBytes, allocBytes, asyncStrCloser, bytesCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else
    return g__inbytes(h, xc, a1, a2);
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

static taskState lineChar(ioPo in, asyncPo async) {
  codePoint cp;
  retCode ret = inChar(in, &cp);
  if (ret == Ok) {
    const char *match = "\n\r";
    if (uniIndexOf(match, 2, 0, cp) >= 0)
      return succeeded;
    else {
      outChar(async->buffer, cp);
      return running;
    }
  } else if (ret == Eof) // Special case of line at end of file
    return succeeded;
  else {
    async->ret = ret;
    return failure;
  }
}

static retCode lineCleanup(asyncPo async, retCode ret) {
  switch (ret) {
    case Eof:
      if (strBufferLength(O_BUFFER(async->buffer)) > 0)
        return Ok;
      else
        return Eof;
    default:
      return ret;
  }
}

ReturnStatus g__inline_async(heapPo h, termPo xc, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  strBufferPo buffer = newStringBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(lineChar, allocStr, asyncStrCloser, lineCleanup, -1, O_IO(newStringBuffer()));

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok)
        return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else
    return g__inline(h, xc, a1);
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

static taskState fileChar(ioPo in, asyncPo async) {
  codePoint cp;
  retCode ret = inChar(in, &cp);
  if (ret == Ok) {
    outChar(async->buffer, cp);
    return running;
  } else if (ret == Eof)
    return succeeded; // Signal that we are done
  else {
    async->ret = ret;
    return failure;
  }
}

static void asyncFileCloser(ioPo io, asyncPo async) {
  closeIo(async->buffer);
  closeIo(io);
  freePool(asyncPool, async);
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

      asyncPo async = newAsyncTask(fileChar, allocStr, asyncFileCloser, lineCleanup, -1, O_IO(newStringBuffer()));

      if (ret == Ok) {
        futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

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

static retCode grabAsync(ioPo io, AsyncStruct *async) {
  retCode ret = Ok;
  while (ret == Ok) {
    switch ((ret = isInputReady(io, 1))) {
      case Ok: {
        switch (async->next(io, async)) {
          case running:
            continue;
          case succeeded:
            return Ok;
          case failure:
            return async->ret;
          case waiting:
            enqueueRead(O_FILE(io), Null, Null);
            return Fail;
          case notStarted:
            return Fail;
        }
      }
      case Fail: {
        // We have to re-enqueue the input
        enqueueRead(O_FILE(io), Null, Null);
        return Fail;
      }
      default:
        return async->cleanup(async, ret);
    }
  }
  return ret;
}

retCode pollInput(futurePo ft, heapPo h, void *cl, void *cl2) {
  filePo f = O_FILE(cl);
  asyncPo async = (asyncPo) cl2;
  switch (asyncRdStatus(f)) {
    case inProgress:
      return Fail;
    case completed: {
      retCode ret = grabAsync(O_IO(f), async);

      switch (ret) {
        case Ok: {
          int root = gcAddRoot(h, (ptrPo) &ft);

          termPo line = async->alloc(h, async);
          gcReleaseRoot(h, root);

          async->close(O_IO(f), async);
          return resolveFuture(ft, line);
        }
        default:
          async->close(O_IO(f), async);
          return rejectFuture(ft, ioErrorCode(ret));
        case Fail:
          return Ok;
      }
    }
    case canceled: {
      async->close(O_IO(f), async);
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      async->close(O_IO(f), async);
      return rejectFuture(ft, eIOERROR);
    }
  }
}
