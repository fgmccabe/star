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
#include "globals.h"

ReturnStatus g__close(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  return rtnStatus(p, closeFile(ioChannel(chnl)), "_close");
}

ReturnStatus g__end_of_file(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isFileAtEof(ioChannel(chnl)) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g__ready_to_read(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isInReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g__ready_to_write(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isOutReady(O_FILE(ioChannel(chnl))) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .result=Rs};

  return ret;
}

ReturnStatus g__inchar(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));

  codePoint cp;
  retCode ret = inChar(io, &cp);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateInteger(processHeap(p), cp)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__inchars(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  integer limit = integerVal(Arg2);

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

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__inbyte(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));

  byte b;
  retCode ret = inByte(io, &b);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateInteger(processHeap(p), b)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}
//
//ReturnStatus g__inbytes(processPo p, ptrPo tos) {
//  termPo Arg1 = tos[0];
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
//    heapPo H = processHeap(p);
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

ReturnStatus g__intext(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  integer mlen;
  const char *match = strVal(Arg2, &mlen);

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

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__inline(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));
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

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .result=voidEnum};
    return rt;
  }
}

ReturnStatus g__get_file(processPo p, ptrPo tos) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(tos[0]), fn, NumberOf(fn));

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

      ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(processHeap(p), text, length)};
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

ReturnStatus g__outchar(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  codePoint cp = (codePoint) integerVal(Arg2);

  return rtnStatus(p, outChar(io, cp), "outchar");
}

ReturnStatus g__outbyte(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  integer cp = integerVal(Arg2);

  return rtnStatus(p, outByte(io, (byte) cp), "outbyte");
}

ReturnStatus g__outbytes(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  termPo data = Arg2;
  retCode ret = Ok;

  while(ret==Ok && isCons(data)){
    byte b = (byte)integerVal(consHead(C_NORMAL(data)));
    ret = outByte(io,b);
    data = consTail(C_NORMAL(data));
  }

  return rtnStatus(p, ret, "outbytes");
}

ReturnStatus g__outtext(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  integer length;
  const char *text = strVal(Arg2, &length);
  retCode ret = Ok;

  integer pos = 0;

  while (ret == Ok && pos < length) {
    integer actual = length;
    ret = outBytes(io, (byte *) &text[pos], length, &actual);
    pos += actual;
  }

  return rtnStatus(p, ret, "outtext");
}

ReturnStatus g__show(processPo p, ptrPo tos) {
  termPo Arg = tos[0];
  integer length;
  const char *text = strVal(Arg, &length);
  retCode ret = outMsg(logFile, "%S\n%_", text, length);

  return rtnStatus(p, ret, "_show");
}

ReturnStatus g__put_file(processPo p, ptrPo tos) {
  char fn[MAXFILELEN];

  copyChars2Buff(C_STR(tos[0]), fn, NumberOf(fn));

  ioPo io = openOutFile(fn, utf8Encoding);
  if (io != Null) {
    integer tLen;
    const char *txt = strVal(tos[1], &tLen);

    retCode ret = outText(io, txt, tLen);
    closeFile(O_IO(io));

    ReturnStatus rt = {.ret=ret, .result=unitEnum};
    return rt;
  } else {
    ReturnStatus rt = {.ret=Error, .result=eNOTFND};
    return rt;
  }
}

ReturnStatus g__logmsg(processPo p, ptrPo tos) {
  integer length;
  const char *text = strVal(tos[0], &length);
  retCode ret = logMsg(logFile, "%S", (char *) text, length);

  return rtnStatus(p, ret, "logmsg");
}

ReturnStatus g__stdfile(processPo p, ptrPo tos) {
  integer fNo = integerVal(tos[0]);

  ReturnStatus rt = {.ret=Ok};

  switch (fNo) {
    case 0:
      rt.result = (termPo) stdInChnl(processHeap(p));
      return rt;
    case 1:
      rt.result = (termPo) stdOutChnl(processHeap(p));
      return rt;
    case 2:
    default:
      rt.result = (termPo) stdErrChnl(processHeap(p));
      return rt;
  }
}

ReturnStatus g__fposition(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));
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

  ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateInteger(processHeap(p), pos)};
  return rt;
}

ReturnStatus g__fseek(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  filePo io = O_FILE(ioChannel(C_IO(Arg1)));
  integer pos = integerVal(Arg2);

  return rtnStatus(p, fileSeek(io, pos), "_fseek");
}

ReturnStatus g__setfileencoding(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo io = ioChannel(C_IO(Arg1));
  integer enc = integerVal(Arg2);
  setEncoding(io, (ioEncoding) enc);

  return rtnStatus(p, Ok, "_setfileencoding");
}

ReturnStatus g__flush(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  return rtnStatus(p, flushFile(ioChannel(chnl)), "flushing problem");
}

ReturnStatus g__flushall(processPo p, ptrPo tos) {
  flushOut();
  return (ReturnStatus){.ret=Ok, .result=voidEnum};
}
