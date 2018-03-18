//
// Created by Francis McCabe on 3/5/18.
//


#include <str.h>
#include <assigns.h>
#include <arith.h>
#include <stringBuffer.h>
#include <ldap.h>
#include <array.h>
#include <arithP.h>
#include "ioops.h"
#include "globals.h"

ReturnStatus g__close(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  return rtnStatus(p, closeFile(ioChannel(chnl)), "_close");
}

ReturnStatus g__end_of_file(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isFileAtEof(ioChannel(chnl)) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__ready_to_read(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isInReady(ioChannel(chnl)) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__ready_to_write(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isOutReady(ioChannel(chnl)) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__inchar(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));

  codePoint cp;
  retCode ret = inChar(io, &cp);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateInteger(processHeap(p), cp)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__inchars(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer limit = integerVal(tos[0]);

  bufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (limit-- > 0 && ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Ok) {
    integer length;
    char *text = getTextFromBuffer(&length, buffer);

    ReturnStatus rt = {.ret=Ok, .rslt=(termPo)allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__inbyte(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));

  byte b;
  retCode ret = inByte(io, &b);
  if (ret == Ok) {
    ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateInteger(processHeap(p), b)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__inbytes(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer limit = integerVal(tos[0]);

  bufferPo buffer = newStringBuffer();
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
    char *text = getTextFromBuffer(&length, buffer);

    heapPo H = processHeap(p);
    listPo lst = allocateList(H, length, True);
    int root = gcAddRoot(H, (ptrPo) &lst);

    for (long ix = 0; ix < length; ix++) {
      byte b = (byte) text[ix];
      termPo bt = (termPo) allocateInteger(H, (integer) b);
      setNthEl(lst, ix, bt);
    }

    closeFile(O_IO(buffer));
    gcReleaseRoot(H, 0);

    ReturnStatus rt = {.ret=Ok, .rslt=(termPo) lst};
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__intext(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer mlen;
  const char *match = stringVal(tos[0], &mlen);

  bufferPo buffer = newStringBuffer();

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
    char *text = getTextFromBuffer(&length, buffer);

    ReturnStatus rt = {.ret=Ok, .rslt=(termPo)allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__inline(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));
  const char *match = "\n\r";
  integer mlen = uniStrLen(match);

  bufferPo buffer = newStringBuffer();

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
    char *text = getTextFromBuffer(&length, buffer);

    ReturnStatus rt = {.ret=Ok, .rslt=(termPo)allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__get_file(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[0]));

  bufferPo buffer = newStringBuffer();

  retCode ret = Ok;
  while (ret == Ok) {
    codePoint cp;
    ret = inChar(io, &cp);
    if (ret == Ok)
      ret = outChar(O_IO(buffer), cp);
  }

  if (ret == Eof) {
    integer length;
    char *text = getTextFromBuffer(&length, buffer);

    ReturnStatus rt = {.ret=Ok, .rslt=(termPo)allocateString(processHeap(p), text, length)};
    closeFile(O_IO(buffer));
    return rt;
  } else {
    closeFile(O_IO(buffer));
    ReturnStatus rt = {.ret=ret, .rslt=voidEnum};
    return rt;
  }
}

ReturnStatus g__outchar(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  codePoint cp = (codePoint) integerVal(tos[0]);

  return rtnStatus(p, outChar(io, cp), "outchar");
}

ReturnStatus g__outbyte(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer cp = integerVal(tos[0]);

  return rtnStatus(p, outByte(io, (byte) cp), "outbyte");
}

ReturnStatus g__outbytes(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  listPo data = C_LIST(tos[0]);
  retCode ret = Ok;

  for (integer ix = 0; ret == Ok && ix < listSize(data); ix++) {
    byte b = (byte) integerVal(nthEl(data, ix));
    ret = outByte(io, b);
  }

  return rtnStatus(p, ret, "outbytes");
}

ReturnStatus g__outtext(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer length;
  const char *text = stringVal(tos[0], &length);
  retCode ret = Ok;

  integer pos = 0;

  while (ret == Ok && pos < length) {
    integer actual = length;
    ret = outBytes(io, (byte *) &text[pos], length, &actual);
    pos += actual;
  }

  return rtnStatus(p, ret, "outtext");
}

ReturnStatus g__logmsg(processPo p, ptrPo tos) {
  integer length;
  const char *text = stringVal(tos[0], &length);
  retCode ret = outText(logFile, text, length);

  return rtnStatus(p, ret, "logmsg");
}

ReturnStatus g__stdfile(processPo p, ptrPo tos) {
  integer fNo = integerVal(tos[0]);

  ReturnStatus rt = {.ret=Ok};

  switch (fNo) {
    case 0:
      rt.rslt = (termPo) stdInChnl(processHeap(p));
      return rt;
    case 1:
      rt.rslt = (termPo) stdOutChnl(processHeap(p));
      return rt;
    case 2:
    default:
      rt.rslt = (termPo) stdErrChnl(processHeap(p));
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

  ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateInteger(processHeap(p), pos)};
  return rt;
}

ReturnStatus g__fseek(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer pos = integerVal(tos[0]);

  return rtnStatus(p, ioSeek(io, pos), "_fseek");
}

ReturnStatus g__setfileencoding(processPo p, ptrPo tos) {
  ioPo io = ioChannel(C_IO(tos[1]));
  integer enc = integerVal(tos[0]);
  setEncoding(io, (ioEncoding) enc);

  return rtnStatus(p, Ok, "_setfileencoding");
}

ReturnStatus g__flush(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  return rtnStatus(p, flushFile(ioChannel(chnl)), "flushing problem");
}

ReturnStatus g__flushall(processPo p, ptrPo tos) {
  flushOut();
  ReturnStatus ret = {.ret=Ok, .rslt=voidEnum};
  return ret;
}



