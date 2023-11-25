//
// Created by Francis McCabe on 3/5/18.
//


#include <strings.h>
#include <assigns.h>
#include <arith.h>
#include <stringBuffer.h>
#include <arithP.h>
#include <errorCodes.h>
#include <cons.h>
#include "ioops.h"
#include "stack.h"
#include "globals.h"
#include "consP.h"
#include "single.h"

ReturnStatus g__close(heapPo h, termPo xc, termPo a1) {
  if (closeFile(ioChannel(C_IO(a1))) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__end_of_file(heapPo h, termPo a1) {
  termPo Rs = (isFileAtEof(ioChannel(C_IO(a1))) == Eof ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

static retCode fillFuture(ioPo f, void *cl) {
  singlePo fut = C_SINGLE((termPo) cl);
  integer count = enqueuedCount(f);
  heapPo currentHeap;

  strBufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (count-- > 0 && ret == Ok) {
    codePoint cp;
    ret = inChar(f, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    ret = setSingle(currentHeap, fut, allocateString(currentHeap, text, length));

    closeFile(O_IO(buffer));
    return ret;
  } else {
    closeFile(O_IO(buffer));
    return ret;
  }
}

ReturnStatus g__enqueue_read(heapPo h, termPo a1, termPo a2) {
  ioChnnlPo chnl = C_IO(a1);
  integer count = integerVal(a2);

  singlePo ft = makeSingle(h);

  retCode ret = enqueueRead(ioChannel(chnl), count, fillFuture, ft);
  return (ReturnStatus) {.ret=ret, .result=(termPo) ft};
}

ReturnStatus g__ready_to_read(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  termPo Rs = (isInReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__ready_to_write(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  termPo Rs = (isOutReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__inchar(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  codePoint cp;
  retCode ret = inChar(io, &cp);
  if (ret == Ok) {
    return (ReturnStatus) {.ret=Ok, .result=makeInteger(cp)};
  } else {
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
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
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__inbyte(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  byte b;
  retCode ret = inByte(io, &b);
  if (ret == Ok) {
    return (ReturnStatus) {.ret=Ok, .result=makeInteger(b)};
  } else {
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
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

    termPo lst = nilEnum;
    int root = gcAddRoot(h, (ptrPo) &lst);

    for (integer ix = length - 1; ix >= 0; ix--) {
      termPo b = makeInteger((byte) text[ix]);
      lst = (termPo) allocateCons(h, b, lst);
    }

    closeFile(O_IO(buffer));
    gcReleaseRoot(h, root);

    return (ReturnStatus) {.ret=Ok, .result=lst};
  } else {
    closeFile(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__intext(heapPo h, termPo a1, termPo a2) {
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

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(h, text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__inline(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));
  const char *match = "\n\r";
  integer mlen = uniStrLen(match);

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

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(h, text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__inline_async(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));
  retCode ret = configureIo(O_FILE(io), enableAsynch);

  const char *match = "\n\r";
  integer mlen = uniStrLen(match);

  strBufferPo buffer = newStringBuffer();

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

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(buffer, &length);

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(h, text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__get_file(heapPo h, termPo xc, termPo a1) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(a1), fn, NumberOf(fn));

  ioPo io = openInFile(fn, utf8Encoding);
  if (io != Null) {
    strBufferPo buffer = newStringBuffer();

    retCode ret = Ok;
    while (ret == Ok) {
      codePoint cp;
      ret = inChar(io, &cp);
      if (ret == Ok)
        ret = outChar(O_IO(buffer), cp);
    }

    if (ret == Eof) {
      integer length;
      char *text = getTextFromBuffer(buffer, &length);

      ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(h, text, length)};
      closeFile(O_IO(buffer));
      return rt;
    } else {
      closeFile(O_IO(buffer));
      return (ReturnStatus) {.ret=ret, .result=voidEnum};
    }
  } else {
    return (ReturnStatus) {.ret=Error, .result=eNOTFND};
  }
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
    closeFile(O_IO(io));

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
  if (flushFile(ioChannel(C_IO(a1))) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eIOERROR};
}

ReturnStatus g__flushall(heapPo h, termPo a1) {
  flushOut();
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}
