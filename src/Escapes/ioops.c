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

ReturnStatus g__close(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  return rtnStatus(h, closeFile(ioChannel(chnl)), "_close");
}

ReturnStatus g__end_of_file(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  termPo Rs = (isFileAtEof(ioChannel(chnl)) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g_enqueue_read(heapPo h, termPo a1, termPo a2, termPo a3) {
  ioChnnlPo chnl = C_IO(a1);
  integer count = integerVal(a2);
  stackPo tskRef = C_STACK(a3);

  retCode ret = enqueueRead(ioChannel(chnl), count, Null, tskRef);
  ReturnStatus rt = {.ret=ret, .result=voidEnum};
  return rt;
}

ReturnStatus g__ready_to_read(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  termPo Rs = (isInReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g__ready_to_write(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  termPo Rs = (isOutReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g__inchar(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  codePoint cp;
  retCode ret = inChar(io, &cp);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .result=makeInteger(cp)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
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
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__inbyte(heapPo h, termPo a1) {
  ioPo io = ioChannel(C_IO(a1));

  byte b;
  retCode ret = inByte(io, &b);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .result=makeInteger(b)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}
//
//ReturnStatus g__inbytes(heapPo h, termPo a1) {
//  termPo Arg1 = a1;
//  termPo Arg2 = tos[1];
//  ioPo io = ioChannel(C_IO(Arg1));
//  integer limit = integerVal(Arg2);
//
//  bufferPo buffer = newStringBuffer();
//  byte bf[MAXLINE];
//
//  retCode ret = Ok;
//
//  while (limit-- > 0 && ret == Ok) {
//    integer cnt;
//    ret = inBytes(io, bf, NumberOf(bf), &cnt);
//
//    if (ret == Ok)
//      ret = outBytes(O_IO(buffer), bf, cnt, &cnt);
//  }
//
//  if (ret == Ok) {
//    integer length;
//    char *text = getTextFromBuffer(buffer, &length);
//
//    arrayPo lst = allocateArray(H, length);
//    int root = gcAddRoot(H, (ptrPo) &lst);
//
//    for (long ix = 0; ix < length; ix++) {
//      byte b = (byte) text[ix];
//      termPo bt = (termPo) allocateInteger(H, (integer) b);
//      setNthEl(lst, ix, bt);
//    }
//
//    closeFile(O_IO(buffer));
//    gcReleaseRoot(H, root);
//
//    ReturnStatus rt = {.ret=Ok, .result=(termPo) lst};
//    return rt;
//  } else {
//    closeFile(O_IO(buffer));
//    ReturnStatus rt = {.ret=ret, .result=voidEnum};
//    return rt;
//  }
//}

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
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
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
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
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
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__get_file(heapPo h, termPo a1) {
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
      ReturnStatus rt = {.ret=ret, .result=voidEnum};
      return rt;
    }
  } else {
    ReturnStatus rt = {.ret=Error, .result=eNOTFND};
    return rt;
  }
}

ReturnStatus g__outchar(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  codePoint cp = (codePoint) integerVal(a2);

  return rtnStatus(h, outChar(io, cp), "outchar");
}

ReturnStatus g__outbyte(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer cp = integerVal(a2);

  return rtnStatus(h, outByte(io, (byte) cp), "outbyte");
}

ReturnStatus g__outbytes(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  retCode ret = Ok;

  while (ret == Ok && isCons(a2)) {
    byte b = (byte) integerVal(consHead(C_NORMAL(a2)));
    ret = outByte(io, b);
    a2 = consTail(C_NORMAL(a2));
  }

  return rtnStatus(h, ret, "outbytes");
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

  return rtnStatus(h, ret, "outtext");
}

ReturnStatus g__show(heapPo h, termPo a1) {
  integer length;
  const char *text = strVal(a1, &length);
  retCode ret = outMsg(logFile, "%S\n%_", text, length);

  return rtnStatus(h, ret, "_show");
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
    ReturnStatus rt = {.ret=Error, .result=eNOTFND};
    return rt;
  }
}

ReturnStatus g__logmsg(heapPo h, termPo a1) {
  integer length;
  const char *text = strVal(a1, &length);
  retCode ret = logMsg(logFile, "%S", (char *) text, length);

  return rtnStatus(h, ret, "logmsg");
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

ReturnStatus g__fseek(heapPo h, termPo a1, termPo a2) {
  filePo io = O_FILE(ioChannel(C_IO(a1)));
  integer pos = integerVal(a2);

  return rtnStatus(h, fileSeek(io, pos), "_fseek");
}

ReturnStatus g__setfileencoding(heapPo h, termPo a1, termPo a2) {
  ioPo io = ioChannel(C_IO(a1));
  integer enc = integerVal(a2);
  setEncoding(io, (ioEncoding) enc);

  return rtnStatus(h, Ok, "_setfileencoding");
}

ReturnStatus g__flush(heapPo h, termPo a1) {
  ioChnnlPo chnl = C_IO(a1);

  return rtnStatus(h, flushFile(ioChannel(chnl)), "flushing problem");
}

ReturnStatus g__flushall(heapPo h, termPo a1) {
  flushOut();
  return (ReturnStatus) {.ret=Ok, .result=voidEnum};
}
