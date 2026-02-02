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
#include "fileops.h"

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

ValueReturn s__close(enginePo P, termPo i){
  retCode ret = closeChannel(C_IO(i));
  if (ret == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(ioErrorCode(ret));
  }
}

ReturnStatus g__close(enginePo P) {
  ValueReturn ret = s__close(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__end_of_file(enginePo P,termPo i){
  return normalReturn(isFileAtEof(ioChannel(C_IO(i))) == Eof ? trueEnum : falseEnum);
}

ReturnStatus g__end_of_file(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__end_of_file(P,i);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__inchar(enginePo P, termPo i){
  codePoint cp;
  retCode ret = inChar(ioChannel(C_IO(i)), &cp);
  switch (ret) {
    case Ok: {
      return normalReturn(makeChar(cp));
    }
    case Eof: {
      return abnormalReturn(eEOF);
    }
    default: {
      return abnormalReturn(eIOERROR);
    }
  }
}

ReturnStatus g__inchar(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__inchar(P,i);
  pshVal(P,ret.value);
  return ret.status;
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
  return makeChar(cp);
}

static retCode oneCleanup(asyncPo sync, retCode ret) {
  return Eof;
}

ValueReturn s__inchar_async(enginePo P, termPo c){
  ioChnnlPo chnl = C_IO(c);
  ioPo io = ioChannel(chnl);
  heapPo h = processHeap(P);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);
    asyncPo async = newAsyncTask(oneChar, allocChar, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
	return normalReturn((termPo)ft);
      } else {
	return abnormalReturn(ioErrorCode(ret));
      }
    }
    return abnormalReturn(eNOPERM);
  } else {
    codePoint cp;
    retCode ret = inChar(io, &cp);
    if (ret == Ok)
      return normalReturn((termPo)makeResolvedFuture(h, makeChar(cp), isAccepted));
    else
      return normalReturn((termPo) makeResolvedFuture(h, ioErrorCode(ret), isRejected));
  }
}

ReturnStatus g__inchar_async(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__inchar_async(P,i);
  pshVal(P,ret.value);
  return ret.status;
}  

ValueReturn s__inchars(enginePo P,termPo i,termPo l) {
  ioPo io = ioChannel(C_IO(i));
  integer limit = integerVal(l);

  strBufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (limit-- > 0 && ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Ok) {
    termPo text = allocateFromStrBuffer(processHeap(P), buffer);
    closeIo(O_IO(buffer));
    return normalReturn(text);
  } else {
    closeIo(O_IO(buffer));
    return abnormalReturn(ioErrorCode(ret));
  }
}

ReturnStatus g__inchars(enginePo P) {
  termPo i = popVal(P);
  termPo l = popVal(P);
  ValueReturn ret = s__inchars(P,i,l);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__inchars_async(enginePo P, termPo i, termPo l){
  ioChnnlPo chnl = C_IO(i);
  integer limit = integerVal(l);

  ioPo io = ioChannel(chnl);
  strBufferPo buffer = newStringBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextChars, allocStr, asyncStrCloser, lineCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
	return normalReturn((termPo)ft);
      }
    }
    return abnormalReturn(ioErrorCode(ret));
  } else {
    retCode ret = Ok;
    while (limit-- > 0 && ret == Ok) {
      codePoint cp;
      ret = inChar(io, &cp);
      if (ret == Ok)
        ret = outChar(O_IO(buffer), cp);
    }

    if (ret == Ok) {
      termPo text = allocateFromStrBuffer(processHeap(P), buffer);
      closeIo(O_IO(buffer));
      return normalReturn(text);
    } else {
      closeIo(O_IO(buffer));
      return abnormalReturn(ioErrorCode(ret));
    }
  }
}

ReturnStatus g__inchars_async(enginePo P) {
  termPo i = popVal(P);
  termPo l = popVal(P);

  ValueReturn ret = s__inchars_async(P,i,l);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__inbyte(enginePo P, termPo i){
  ioPo io = ioChannel(C_IO(i));

  byte b;
  switch (inByte(io, &b)) {
  case Ok:
    return normalReturn(makeInteger(b));
  case Eof:
    return abnormalReturn(eEOF);
  default:
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__inbyte(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__inbyte(P,i);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__inbyte_async(enginePo P, termPo i){
  ioPo io = ioChannel(C_IO(i));
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);
    asyncPo async = newAsyncTask(oneByte, allocByte, asyncCloser, oneCleanup, -1, Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
	return normalReturn((termPo)ft);
      }
    }
    return abnormalReturn(ioErrorCode(ret));
  } else {
    byte b;
    retCode ret = inByte(io, &b);
    switch (ret) {
      case Ok: {
	return normalReturn(makeInteger(b));
      }
      case Eof:
	return abnormalReturn(eEOF);
      default:
	return abnormalReturn(eIOERROR);
    }
  }
}

ReturnStatus g__inbyte_async(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__inbyte_async(P,i);
  pshVal(P,ret.value);
  return ret.status;
}

termPo makeByte(heapPo h, integer ix, void *cl) {
  char *str = (char *) cl;
  integer ch = (byte) str[ix];
  return makeInteger(ch);
}

ValueReturn s__inbytes(enginePo P,termPo i, termPo l) {
  ioPo io = ioChannel(C_IO(i));
  integer limit = integerVal(l);

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

    termPo vect = makeVector(processHeap(P), length, makeByte, (void *) text);
    return normalReturn(vect);
  } else {
    closeIo(O_IO(buffer));
    return abnormalReturn(ioErrorCode(ret));
  }
}

ReturnStatus g__inbytes(enginePo P) {
  termPo i = popVal(P);
  termPo l = popVal(P);
  ValueReturn ret = s__inbytes(P, i, l);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__inbytes_async(enginePo P,termPo i, termPo l) {
  ioChnnlPo chnl = C_IO(i);
  integer limit = integerVal(l);

  ioPo io = ioChannel(chnl);
  byteBufferPo buffer = newByteBuffer();

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(nextBytes, allocBytes, asyncStrCloser, bytesCleanup, limit, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
	return normalReturn((termPo)ft);
      } else {
	return abnormalReturn(ioErrorCode(ret));
      }
    }
    return abnormalReturn(eNOPERM);
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__inbytes_async(enginePo P) {
  termPo i = popVal(P);
  termPo l = popVal(P);
  ValueReturn ret = s__inbytes_async(P, i, l);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__inline(enginePo P,termPo i) {
  ioPo io = ioChannel(C_IO(i));

  strBufferPo buffer = newStringBuffer();

  retCode ret = grabLine(io, buffer);

  switch (ret) {
    case Ok: {
      integer length;
      char *text = getTextFromBuffer(buffer, &length);

      termPo line = allocateString(processHeap(P), text, length);
      closeIo(O_IO(buffer));
      return normalReturn(line);
    }
    case Eof: {
      closeIo(O_IO(buffer));
      return abnormalReturn(eEOF);
    }
    default: {
      closeIo(O_IO(buffer));
      return abnormalReturn(eIOERROR);
    }
  }
}

ReturnStatus g__inline(enginePo P) {
  termPo i = popVal(P);
  ValueReturn ret = s__inline(P,i);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__inline_async(enginePo P,termPo i) {
  ioChnnlPo chnl = C_IO(i);
  ioPo io = ioChannel(chnl);

  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(lineChar, allocStr, asyncStrCloser, lineCleanup, -1, O_IO(newStringBuffer()));

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollInput, io, async);

      if ((ret = enqueueRead(f, Null, Null)) == Ok) {
        return normalReturn((termPo) ft);
      }
    }
    return abnormalReturn(ioErrorCode(ret));
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__inline_async(enginePo P) {
  termPo i = popVal(P);

  ValueReturn ret = s__inline_async(P,i);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__get_file(enginePo P,termPo f, termPo e) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(f), fn, NumberOf(fn));

  ioEncoding enc = pickEncoding(integerVal(e));

  ioPo io = openInFile(fn, enc);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();

    retCode ret = grabText(io, O_IO(buffer));

    if (ret == Eof) {
      termPo text = allocateFromStrBuffer(processHeap(P), buffer);
      closeIo(O_IO(buffer));
      return normalReturn(text);
    } else {
      closeIo(O_IO(buffer));
      return abnormalReturn(ioErrorCode(ret));
    }
  } else {
    return abnormalReturn(eNOTFND);
  }
}

ReturnStatus g__get_file(enginePo P) {
  termPo p = popVal(P);
  termPo e = popVal(P);
  
  ValueReturn ret = s__get_file(P,p,e);
  pshVal(P,ret.value);
  return ret.status;
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

ValueReturn s__logmsg(enginePo P,termPo m) {
  integer length;
  const char *text = strVal(m, &length);

  logMsg(logFile, "%S", (char *) text, length);
  return normalReturn(unitEnum);
}

ReturnStatus g__logmsg(enginePo P) {
  termPo m = popVal(P);
  
  ValueReturn ret = s__logmsg(P,m);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__display_depth(enginePo P){
  return normalReturn(makeInteger(displayDepth));
}

ReturnStatus g__display_depth(enginePo P) {
  pshVal(P, makeInteger(displayDepth));
  return Normal;
}

ValueReturn s__stdfile(enginePo P, termPo i){
  integer fNo = integerVal(i);

  heapPo h = processHeap(P);
  switch (fNo) {
    case 0:
      return normalReturn((termPo) stdInChnl(h));
    case 1:
      return normalReturn((termPo) stdOutChnl(h));
    case 2:
    default:
      return normalReturn((termPo) stdErrChnl(h));
  }
}

ReturnStatus g__stdfile(enginePo P) {
  ValueReturn ret = s__stdfile(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__fposition(enginePo P,termPo f){
  ioPo io = ioChannel(C_IO(f));
  return normalReturn(makeInteger(ioPos(io)));
}

ReturnStatus g__fposition(enginePo P) {
  ValueReturn ret = s__fposition(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}  

ValueReturn s__fseek(enginePo P,termPo i, termPo p) {
  ioPo io = ioChannel(C_IO(i));
  integer pos = integerVal(p);

  retCode ret = ioSeek(io, pos);

  if (ret == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(ioErrorCode(ret));
  }
}

ReturnStatus g__fseek(enginePo P) {
  ValueReturn ret = s__fposition(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}  

ValueReturn s__fname(enginePo P,termPo i) {
  ioPo io = ioChannel(C_IO(i));

  return normalReturn(allocateCString(processHeap(P), fileName(io)));
}

ReturnStatus g__fname(enginePo P) {
  ValueReturn ret = s__fname(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}  

ValueReturn s__setfileencoding(enginePo P,termPo f, termPo e) {
  ioPo io = ioChannel(C_IO(f));
  integer enc = integerVal(e);
  setEncoding(io, (ioEncoding) enc);
  return normalReturn(unitEnum);
}

ReturnStatus g__setfileencoding(enginePo P) {
  termPo f = popVal(P);
  termPo e = popVal(P);
  
  ValueReturn ret = s__setfileencoding(P, f, e);
  pshVal(P,ret.value);
  return ret.status;
}  

ValueReturn s__flush(enginePo P, termPo f) {
  ioPo io = ioChannel(C_IO(f));
  if (isAFile(O_OBJECT(io)) && flushFile(O_FILE(io)) == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__flush(enginePo P) {
  termPo f = popVal(P);
  ValueReturn ret = s__flush(P, f);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__flushall(enginePo P){
  flushOut();
  return normalReturn(unitEnum);
}

ReturnStatus g__flushall(enginePo P) {
  flushOut();
  pshVal(P, unitEnum);
  return Normal;
}

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

ValueReturn s__waitio(enginePo P, termPo list, termPo t){
  // First count the length of the list
  integer count = 0;
  integer timeOut = integerVal(t);
  walkNormal(list, countIoChnnls, (void *) &count);

  if (count > 0) {
    filePo files[count];

    FileData fd = {.ix = 0, .files = files};
    walkNormal(list, populateFiles, &fd);

    assert(count == fd.ix);

    retCode ret = waitForAsync(files, fd.ix, timeOut);

    if (ret == Ok) {
      return normalReturn(trueEnum);
    } else {
      return normalReturn(falseEnum);
    }
  } else {
    return normalReturn(trueEnum);
  }
}

ReturnStatus g__waitIo(enginePo P) {
  termPo l = popVal(P);
  termPo t = popVal(P);

  ValueReturn ret = s__waitio(P, l, t);
  pshVal(P,ret.value);
  return ret.status;
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
