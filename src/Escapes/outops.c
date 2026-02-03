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
#include "escape.h"
#include "fileops.h"

static retCode pollOutput(futurePo ft, heapPo h, void *cl, void *cl2);

ValueReturn s__outchar(enginePo P, termPo i, termPo c) {
  ioPo io = ioChannel(C_IO(i));
  codePoint cp = charVal(c);

  if (outChar(io, cp) == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__outchar(enginePo P) {
  termPo i = popVal(P);
  termPo c = popVal(P);
  ValueReturn ret = s__outchar(P, i, c);
  pshVal(P, ret.value);
  return ret.status;
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

ValueReturn s__outchar_async(enginePo P, termPo o, termPo c) {
  ioChnnlPo chnl = C_IO(o);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(wrChar, allocUnit, asyncCloser, wrCleanup, charVal(c), Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollOutput, io, async);
      return normalReturn((termPo) ft);
    }
    return abnormalReturn(eNOPERM);
  }
  return abnormalReturn(eINVAL);
}

ReturnStatus g__outchar_async(enginePo P) {
  termPo i = popVal(P);
  termPo c = popVal(P);
  ValueReturn ret = s__outchar_async(P, i, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__outbyte(enginePo P, termPo i, termPo c) {
  ioPo io = ioChannel(C_IO(i));
  integer cp = integerVal(popVal(P));

  if (outByte(io, cp) == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__outbyte(enginePo P) {
  termPo i = popVal(P);
  termPo c = popVal(P);
  ValueReturn ret = s__outbyte(P, i, c);
  pshVal(P, ret.value);
  return ret.status;
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

ValueReturn s__outbyte_async(enginePo P, termPo o, termPo b) {
  ioChnnlPo chnl = C_IO(o);
  ioPo io = ioChannel(chnl);
  if (isAFile(O_OBJECT(io))) {
    filePo f = O_FILE(io);

    retCode ret = enableASynch(f);

    asyncPo async = newAsyncTask(wrByte, allocUnit, asyncCloser, wrCleanup, integerVal(b), Null);

    if (ret == Ok) {
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollOutput, io, async);
      return normalReturn((termPo) ft);
    }
    return abnormalReturn(eNOPERM);
  }
  return abnormalReturn(eINVAL);
}

ReturnStatus g__outbyte_async(enginePo P) {
  termPo i = popVal(P);
  termPo c = popVal(P);
  ValueReturn ret = s__outbyte_async(P, i, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__outbytes(enginePo P, termPo i, termPo list) {
  ioPo io = ioChannel(C_IO(i));

  retCode ret = Ok;

  while (ret == Ok && isCons(list)) {
    byte b = (byte) integerVal(consHead(C_NORMAL(list)));
    ret = outByte(io, b);
    list = consTail(C_NORMAL(list));
  }

  if (ret == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__outbytes(enginePo P) {
  termPo i = popVal(P);
  termPo list = popVal(P);
  ValueReturn ret = s__outbytes(P, i, list);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__outtext(enginePo P, termPo i, termPo t) {
  ioPo io = ioChannel(C_IO(i));
  integer length;
  const char *text = strVal(t, &length);
  retCode ret = Ok;

  integer pos = 0;

  while (ret == Ok && pos < length) {
    integer actual = length;
    ret = outBytes(io, (byte *) &text[pos], length, &actual);
    pos += actual;
  }

  if (ret == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eIOERROR);
  }
}

ReturnStatus g__outtext(enginePo P) {
  termPo i = popVal(P);
  termPo m = popVal(P);
  ValueReturn ret = s__outtext(P, i, m);
  pshVal(P, ret.value);
  return ret.status;
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

ValueReturn s__outtext_async(enginePo P, termPo o, termPo a2) {
  ioChnnlPo chnl = C_IO(o);
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
      futurePo ft = makeFuture(processHeap(P), voidEnum, pollOutput, io, async);
      return normalReturn((termPo) ft);
    }
    return abnormalReturn(eNOPERM);
  }
  return abnormalReturn(eINVAL);
}

ReturnStatus g__outtext_async(enginePo P) {
  termPo i = popVal(P);
  termPo c = popVal(P);
  ValueReturn ret = s__outtext_async(P, i, c);
  pshVal(P, ret.value);
  return ret.status;
}


ValueReturn s__show(enginePo P, termPo t) {
  integer length;
  const char *text = strVal(t, &length);

  outMsg(logFile, "%S\n%_", text, length);

  return normalReturn(unitEnum);
}

ReturnStatus g__show(enginePo P) {
  termPo m = popVal(P);
  ValueReturn ret = s__show(P, m);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__put_file(enginePo P, termPo f, termPo e, termPo l) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(f), fn, NumberOf(fn));
  ioEncoding enc = pickEncoding(integerVal(e));

  ioPo io = openOutFile(fn, enc);
  if (io != Null) {
    integer tLen;
    const char *txt = strVal(l, &tLen);

    retCode ret = outText(io, txt, tLen);
    closeIo(O_IO(io));

    if (ret == Ok) {
      return normalReturn(voidEnum);
    }
    return abnormalReturn(eIOERROR);
  }
  return abnormalReturn(eNOPERM);
}

ReturnStatus g__put_file(enginePo P) {
  termPo f = popVal(P);
  termPo e = popVal(P);
  termPo l = popVal(P);

  ValueReturn ret = s__put_file(P, f, e, l);
  pshVal(P, ret.value);
  return ret.status;
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
