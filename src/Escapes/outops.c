//
// Created by Francis McCabe on 2/10/24.
//
#include <assert.h>
#include <stdlib.h>
#include "ioops.h"
#include "engine.h"
#include "future.h"
#include "globals.h"
#include "consP.h"
#include "charP.h"
#include "vectP.h"
#include "futureP.h"
#include "errorCodes.h"
#include "arith.h"

static retCode pollOutput(futurePo ft, heapPo h, void *cl, void *cl2);

ReturnStatus g__outchar(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  codePoint cp = charVal(a2);

  if (outChar(io, cp) == Ok)
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
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

ReturnStatus g__outchar_async(heapPo h, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(wrChar, allocUnit, asyncCloser, wrCleanup, charVal(a2), Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Normal, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=Abnormal, .result=eNOPERM};
  } else
    return g__outchar(h, a1, a2);
}

ReturnStatus g__outbyte(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer cp = integerVal(a2);

  if (outByte(io, cp) == Ok)
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
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

ReturnStatus g__outbyte_async(heapPo h, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(wrByte, allocUnit, asyncCloser, wrCleanup, integerVal(a2), Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Normal, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=Abnormal, .result=eNOPERM};
  } else
    return g__outchar(h, a1, a2);
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
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
}

ReturnStatus g__outtext(heapPo h, termPo a1, termPo a2) {
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
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
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

static void textCloser(ioPo io, asyncPo async) {
  free((void *) async->data);
  closeIo(async->buffer);
  asyncCloser(io, async);
}

ReturnStatus g__outtext_async(heapPo h, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    integer len;
    const char *text = strVal(a2, &len);
    char *copy = malloc(len + 1);
    uniNCpy(copy, len + 1, text, len);
    strBufferPo buffer = newReadStringBuffer(copy, len);

    asyncPo async = newAsyncTask(wrText, allocUnit, textCloser, wrCleanup, (integer) copy, O_IO(buffer));

    if (ret == Ok) {
      futurePo ft = makeFuture(h, voidEnum, pollOutput, io, async);

      return (ReturnStatus) {.ret=Normal, .result=(termPo) ft};
    }
    return (ReturnStatus) {.ret=Abnormal, .result=eNOPERM};
  } else
    return g__outchar(h, a1, a2);
}

ReturnStatus g__show(heapPo h, termPo a1) {
  integer length;
  const char *text = strVal(a1, &length);
  outMsg(logFile, "%S\n%_", text, length);
  return (ReturnStatus) {.ret=Normal, .result=unitEnum};
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

    if (ret == Ok)
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
    else
      return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
  } else {
    return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};
  }
}

static void putFileCloser(ioPo io, asyncPo async) {
  closeIo(io);
  free((void *) async->data);
  closeIo(async->buffer);
  asyncCloser(io, async);
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
          async->close(O_IO(f), async);
          return resolveFuture(ft, unitEnum);
        }
        default:
          async->close(O_IO(f), async);
          return rejectFuture(ft, ioErrorCode(async->ret));
        case running:
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
