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

ReturnStatus g__close(processPo P) {
  retCode ret = closeChannel(C_IO(popVal(P)));
  if (ret == Ok) {
    pshVal(P, unitEnum);
    return Normal;
  } else {
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  }
}

ReturnStatus g__end_of_file(processPo P) {
  termPo Rs = (isFileAtEof(ioChannel(C_IO(popVal(P)))) == Eof ? trueEnum : falseEnum);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__inchar(processPo P) {
  codePoint cp;
  retCode ret = inChar(ioChannel(C_IO(popVal(P))), &cp);
  switch (ret) {
    case Ok: {
      pshVal(P, allocateCharacter(cp));
      return Normal;
    }
    case Eof: {
      pshVal(P, eEOF);
      return Abnormal;
    }
    default: {
      pshVal(P, eIOERROR);
      return Abnormal;
    }
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

ReturnStatus g__inchar_async(processPo P) {
  ioChnnlPo chnl = C_IO(popVal(P));
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(oneChar, allocChar, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(currentHeap, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        pshVal(P, (termPo) ft);
        return Normal;
      } else{
        pshVal(P, ioErrorCode(ret));
        return Abnormal;
      }
    }
    pshVal(P, eNOPERM);
    return Abnormal;
  } else {
    codePoint cp;
    retCode ret = inChar(io, &cp);
    if (ret == Ok)
      pshVal(P, (termPo) makeResolvedFuture(currentHeap, allocateCharacter(cp), isAccepted));
    else
      pshVal(P, (termPo) makeResolvedFuture(currentHeap, ioErrorCode(ret), isRejected));
    return Normal;
  }
}

ReturnStatus g__inchars(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  integer limit = integerVal(popVal(P));

  strBufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (limit-- > 0 && ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Ok) {
    pshVal(P, allocateFromStrBuffer(currentHeap, buffer));
    closeIo(O_IO(buffer));
    return Normal;
  } else {
    closeIo(O_IO(buffer));
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  }
}

static taskState nextChars(ioPo in, asyncPo async) {
  if (async->data > 0) {
    codePoint cp;
    retCode ret = inChar(in, &cp);
    if (ret == Ok) {
      outChar(async->buffer, cp);
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

ReturnStatus g__inchars_async(processPo P) {
  ioChnnlPo chnl = C_IO(popVal(P));
  integer limit = integerVal(popVal(P));

  ioPo io = ioChannel(chnl);
  strBufferPo buffer = newStringBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextChars, allocStr, asyncStrCloser, lineCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(currentHeap, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        pshVal(P, (termPo) ft);
        return Normal;
      }
    }
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  } else {
    retCode ret = Ok;
    while (limit-- > 0 && ret == Ok) {
      codePoint cp;
      ret = inChar(io, &cp);
      if (ret == Ok)
        ret = outChar(O_IO(buffer), cp);
    }

    if (ret == Ok) {
      pshVal(P, allocateFromStrBuffer(currentHeap, buffer));
      closeIo(O_IO(buffer));
      return Normal;
    } else {
      closeIo(O_IO(buffer));
      pshVal(P, ioErrorCode(ret));
      return Abnormal;
    }
  }
}

ReturnStatus g__inbyte(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));

  byte b;
  retCode ret = inByte(io, &b);
  switch (ret) {
    case Ok:
      pshVal(P, makeInteger(b));
      return Normal;
    case Eof:
      pshVal(P, eEOF);
      return Abnormal;
    default:
      pshVal(P, eIOERROR);
      return Abnormal;
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

ReturnStatus g__inbyte_async(processPo P) {
  ioChnnlPo chnl = C_IO(popVal(P));
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);
    asyncPo async = newAsyncTask(oneByte, allocByte, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(currentHeap, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        pshVal(P, (termPo) ft);
        return Normal;
      }
    }
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  } else {
    byte b;
    retCode ret = inByte(io, &b);
    switch (ret) {
      case Ok: {
        pshVal(P, makeInteger(b));
        return Normal;
      }
      case Eof:
        pshVal(P, eEOF);
        return Abnormal;
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

termPo makeByte(heapPo h, integer ix, void *cl) {
  char *str = (char *) cl;
  integer ch = (byte) str[ix];
  return makeInteger(ch);
}

ReturnStatus g__inbytes(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  integer limit = integerVal(popVal(P));

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

    termPo vect = makeVector(currentHeap, length, makeByte, (void *) text);
    pshVal(P, vect);
    return Normal;
  } else {
    closeIo(O_IO(buffer));
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  }
}

static taskState nextBytes(ioPo in, asyncPo async) {
  if (async->data > 0) {
    byte by;
    retCode ret = inByte(in, &by);
    if (ret == Ok) {
      appendByteToBuffer(O_BYTEBUFFER(async->buffer), by);
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

ReturnStatus g__inbytes_async(processPo P) {
  ioChnnlPo chnl = C_IO(popVal(P));
  integer limit = integerVal(popVal(P));

  ioPo io = ioChannel(chnl);
  byteBufferPo buffer = newByteBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextBytes, allocBytes, asyncStrCloser, bytesCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(currentHeap, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        pshVal(P, (termPo) ft);
        return Normal;
      } else{
        pshVal(P, ioErrorCode(ret));
        return Abnormal;
      }
    }
    pshVal(P, eNOPERM);
    return Abnormal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
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

ReturnStatus g__inline(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));

  strBufferPo buffer = newStringBuffer();

  retCode ret = grabLine(io, buffer);

  switch (ret) {
    case Ok: {
      integer length;
      char *text = getTextFromBuffer(buffer, &length);

      pshVal(P, allocateString(currentHeap, text, length));
      closeIo(O_IO(buffer));
      return Normal;
    }
    case Eof: {
      closeIo(O_IO(buffer));
      pshVal(P, eEOF);
      return Abnormal;
    }
    default: {
      closeIo(O_IO(buffer));
      pshVal(P, eIOERROR);
      return Abnormal;
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

ReturnStatus g__inline_async(processPo P) {
  ioChnnlPo chnl = C_IO(popVal(P));
  ioPo io = ioChannel(chnl);

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(lineChar, allocStr, asyncStrCloser, lineCleanup, -1, O_IO(newStringBuffer()));

    if (ret == Ok) {
      futurePo ft = makeFuture(currentHeap, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        pshVal(P, (termPo) ft);
        return Normal;
      }
    }
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
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

ReturnStatus g__get_file(processPo P) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(popVal(P)), fn, NumberOf(fn));

  ioPo io = openInFile(fn, utf8Encoding);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();

    retCode ret = grabText(io, O_IO(buffer));

    if (ret == Eof) {
      termPo text = allocateFromStrBuffer(currentHeap, buffer);
      closeIo(O_IO(buffer));
      pshVal(P, text);
      return Normal;
    } else {
      closeIo(O_IO(buffer));
      pshVal(P, ioErrorCode(ret));
      return Abnormal;
    }
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
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

ReturnStatus g__logmsg(processPo P) {
  integer length;
  const char *text = strVal(popVal(P), &length);

  logMsg(logFile, "%S", (char *) text, length);
  pshVal(P, unitEnum);
  return Normal;
}

ReturnStatus g__display_depth(processPo P) {
  pshVal(P, makeInteger(displayDepth));
  return Normal;
}

ReturnStatus g__stdfile(processPo P) {
  integer fNo = integerVal(popVal(P));

  switch (fNo) {
    case 0:
      pshVal(P, (termPo) stdInChnl(currentHeap));
      return Normal;
    case 1:
      pshVal(P, (termPo) stdOutChnl(currentHeap));
      return Normal;
    case 2:
    default:
      pshVal(P, (termPo) stdErrChnl(currentHeap));
      return Normal;
  }
}

ReturnStatus g__fposition(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  pshVal(P, makeInteger(ioPos(io)));
  return Normal;
}

ReturnStatus g__fseek(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  integer pos = integerVal(popVal(P));

  retCode ret = ioSeek(io, pos);

  if (ret == Ok) {
    pshVal(P, unitEnum);
    return Normal;
  } else {
    pshVal(P, ioErrorCode(ret));
    return Abnormal;
  }
}

ReturnStatus g__fname(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));

  pshVal(P, allocateCString(currentHeap, fileName(io)));
  return Normal;
}

ReturnStatus g__setfileencoding(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  integer enc = integerVal(popVal(P));
  setEncoding(io, (ioEncoding) enc);
  pshVal(P, unitEnum);
  return Normal;
}

ReturnStatus g__flush(processPo P) {
  ioPo io = ioChannel(C_IO(popVal(P)));
  if (isAFile(O_OBJECT(io)) && flushFile(O_FILE(io)) == Ok){
    pshVal(P, unitEnum);
    return Normal;
  }
  else{
    pshVal(P,eIOERROR);
    return Abnormal;
  }
}

ReturnStatus g__flushall(processPo P) {
  flushOut();
  pshVal(P, unitEnum);
  return Normal;}

static retCode countIoChnnls(termPo t, void *cl) {
  if (isIoChannel(t)) {
    filePo f = O_FILE(ioChannel(C_IO(t)));
    if (isInAsync(f)) {
      integer *ix = (integer *) cl;
      (*ix)++;
    }
  }
  return Ok;
}

typedef struct {
  integer ix;
  filePo *files;
} FileData;

static retCode populateFiles(termPo t, void *cl) {
  if (isIoChannel(t)) {
    filePo f = O_FILE(ioChannel(C_IO(t)));
    if (isInAsync(f)) {
      FileData *data = (FileData *) cl;
      data->files[data->ix++] = f;
    }
  }
  return Ok;
}

ReturnStatus g__waitIo(processPo P) {
  // First count the length of the list
  termPo list = popVal(P);
  integer count = 0;
  integer timeOut = integerVal(popVal(P));
  walkNormal(list, countIoChnnls, (void *) &count);

  if (count > 0) {
    filePo files[count];

    FileData fd = {.ix = 0, .files = files};
    walkNormal(list, populateFiles, &fd);

    assert(count == fd.ix);

    retCode ret = waitForAsync(files, fd.ix, timeOut);

    if (ret == Ok){
      pshVal(P,trueEnum);
      return Normal;
    }
    else{
      pshVal(P,falseEnum);
      return Normal;
    }
  } else{
    pshVal(P,trueEnum);
    return Normal;
  }
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
