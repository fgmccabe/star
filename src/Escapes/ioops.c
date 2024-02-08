//
// Created by Francis McCabe on 3/5/18.
//

#include <strings.h>
#include <assigns.h>
#include <arith.h>
#include <arithP.h>
#include <errorCodes.h>
#include <cons.h>
#include <stdlib.h>
#include "future.h"
#include "ioops.h"
#include "globals.h"
#include "consP.h"
#include "charP.h"
#include "vectP.h"
#include "futureP.h"
#include "byteBuffer.h"

static poolPo asyncPool = Null;

static termPo ioErrorCode(retCode ret);

static retCode pollInput(futurePo ft, heapPo h, void *cl, void *cl2);
static retCode pollOutput(futurePo ft, heapPo h, void *cl, void *cl2);

typedef struct asyncStruct_ *asyncPo;
typedef taskState (*nextProc)(ioPo in, asyncPo async);
typedef termPo (*asyncAlloc)(heapPo h, asyncPo async);
typedef void (*asyncClose)(asyncPo async);
typedef retCode (*asyncCleanup)(asyncPo async, retCode ret);

typedef struct asyncStruct_ {
  nextProc next;
  asyncAlloc alloc;
  asyncClose close;
  asyncCleanup cleanup;
  integer data;
  retCode ret;
  taskState state;
  ioPo buffer;
} AsyncStruct;

static taskState pushAsync(ioPo io, AsyncStruct *async);

static retCode lineCleanup(asyncPo async, retCode ret);

void initIoOps() {
  if (asyncPool == Null) {
    asyncPool = newPool(sizeof(AsyncStruct), 16);
  }
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

static void asyncCloser(asyncPo async) {
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

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = oneChar;
    async->buffer = Null;
    async->data = -1;
    async->close = asyncCloser;
    async->alloc = allocChar;
    async->cleanup = oneCleanup;

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

static void asyncStrCloser(asyncPo async) {
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

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = nextChars;
    async->buffer = O_IO(buffer);
    async->data = limit;
    async->close = asyncStrCloser;
    async->alloc = allocStr;
    async->cleanup = lineCleanup;

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
    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = oneByte;
    async->buffer = Null;
    async->data = -1;
    async->close = asyncCloser;
    async->alloc = allocByte;
    async->cleanup = oneCleanup;

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

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = nextBytes;
    async->buffer = O_IO(buffer);
    async->data = limit;
    async->close = asyncStrCloser;
    async->alloc = allocBytes;
    async->cleanup = bytesCleanup;

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

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = lineChar;
    async->buffer = O_IO(newStringBuffer());
    async->data = -1;
    async->close = asyncStrCloser;
    async->alloc = allocStr;
    async->cleanup = lineCleanup;

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

ReturnStatus g__getfile_async(heapPo h, termPo xc, termPo a1) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(a1), fn, NumberOf(fn));

  ioPo io = openInFile(fn, utf8Encoding);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();
    if (isAFile(O_OBJECT(io))) {
      filePo f = O_FILE(io);

      retCode ret = enableASynch(f);

      asyncPo async = (asyncPo) allocPool(asyncPool);
      async->next = fileChar;
      async->buffer = O_IO(newStringBuffer());
      async->data = -1;
      async->close = asyncStrCloser;
      async->alloc = allocStr;
      async->cleanup = lineCleanup;

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

ReturnStatus g__outchar(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  codePoint cp = charVal(a2);

  if (outChar(io, cp) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

static taskState nullWrite(ioPo out, asyncPo async) {
  return succeeded;
}

static taskState wrChar(ioPo out, asyncPo async) {
  codePoint cp = (codePoint) async->data;
  assert(async->state == notStarted);
  retCode ret = outChar(out, cp);
  async->state = succeeded;

  if (ret == Ok)
    return waiting;
  else {
    async->ret = ret;
    return failure;
  }
}

static termPo allocUnit(heapPo h, asyncPo async) {
  return unitEnum;
}

static retCode wrCleanup(asyncPo async, retCode ret) {
  return ret;
}

ReturnStatus g__outchar_async(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = wrChar;
    async->buffer = Null;
    async->data = charVal(a2);
    async->close = asyncCloser;
    async->alloc = allocUnit;
    async->cleanup = wrCleanup;
    async->state = notStarted;

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else
    return g__outchar(h, xc, a1, a2);
}

ReturnStatus g__outbyte(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer cp = integerVal(a2);

  if (outByte(io, cp) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

static taskState wrByte(ioPo out, asyncPo async) {
  byte by = (byte) async->data;
  assert(async->state == notStarted);
  retCode ret = outByte(out, by);
  async->state = succeeded;

  if (ret == Ok)
    return waiting;
  else {
    async->ret = ret;
    return failure;
  }
}

ReturnStatus g__outbyte_async(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = wrByte;
    async->buffer = Null;
    async->data = integerVal(a2);
    async->close = asyncCloser;
    async->alloc = allocUnit;
    async->cleanup = wrCleanup;
    async->state = notStarted;

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else
    return g__outchar(h, xc, a1, a2);
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

static taskState wrText(ioPo out, asyncPo async) {
  strBufferPo buffer = O_BUFFER(async->buffer);
  if (isFileAtEof(O_IO(buffer)) == Eof) {
    async->state = succeeded;
    return waiting;
  } else {
    codePoint cp;
    retCode ret = inChar(O_IO(buffer), &cp);
    if (ret == Ok) {
      ret = outChar(out, cp);
    }

    if (ret == Ok)
      return running;

    else {
      async->ret = ret;
      return failure;
    }
  }
}

static void textCloser(asyncPo async) {
  free((void*)async->data);
  closeIo(async->buffer);
  freePool(asyncPool, async);
}

ReturnStatus g__outtext_async(heapPo h, termPo xc, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    integer len;
    const char *text = strVal(a2, &len);
    char *copy = malloc(len + 1);
    uniNCpy(copy, len+1, text, len);
    strBufferPo buffer = newReadStringBuffer(copy, len);

    asyncPo async = (asyncPo) allocPool(asyncPool);
    async->next = wrText;
    async->buffer = O_IO(buffer);
    async->data = (integer)copy;
    async->close = textCloser;
    async->alloc = allocUnit;
    async->cleanup = wrCleanup;
    async->state = notStarted;

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Ok, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=ret, .result=eNOPERM};
  } else
    return g__outchar(h, xc, a1, a2);
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

          async->close(async);
          return resolveFuture(ft, line);
        }
        default:
          async->close(async);
          return rejectFuture(ft, ioErrorCode(ret));
        case Fail:
          return Ok;
      }
    }
    case canceled: {
      async->close(async);
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      async->close(async);
      return rejectFuture(ft, eIOERROR);
    }
  }
}

static taskState pushAsync(ioPo io, AsyncStruct *async) {
  retCode ret = Ok;
  while (ret == Ok) {
    switch ((ret = isOutputReady(io, 1))) {
      case Ok: {
        switch (async->next(io, async)) {
          case running:
            continue;
          case succeeded:
            return succeeded;
          case failure:
            return failure;
          case waiting:
            enqueueWrite(O_FILE(io), Null, Null);
            return running;
          case notStarted:
            return failure;
        }
      }
      case Fail: {
        // We have to re-enqueue the output
        enqueueWrite(O_FILE(io), Null, Null);
        return running;
      }
      default:
        async->ret = async->cleanup(async, ret);
        return (async->ret == Ok ? succeeded : failure);
    }
  }
  return failure;
}

retCode pollOutput(futurePo ft, heapPo h, void *cl, void *cl2) {
  filePo f = O_FILE(cl);
  asyncPo async = (asyncPo) cl2;
  switch (asyncWrStatus(f)) {
    case inProgress:
      return Fail;
    case completed: {
      taskState ret = (async->state != succeeded ? pushAsync(O_IO(f), async) : succeeded);

      switch (ret) {
        case succeeded: {
          async->close(async);
          return resolveFuture(ft, unitEnum);
        }
        default:
          async->close(async);
          return rejectFuture(ft, ioErrorCode(async->ret));
        case running:
          return Ok;
      }

    }
    case canceled: {
      async->close(async);
      return rejectFuture(ft, canceledEnum);
    }
    case failed: {
      async->close(async);
      return rejectFuture(ft, eIOERROR);
    }
  }
}
