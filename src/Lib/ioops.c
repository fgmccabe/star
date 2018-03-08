//
// Created by Francis McCabe on 3/5/18.
//


#include <str.h>
#include <assigns.h>
#include "iochnnlP.h"
#include "globals.h"

ReturnStatus g__end_of_file(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  termPo Rs = (isFileAtEof(chnl->io) == Eof ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flush(processPo p, ptrPo tos) {
  ioChnnlPo chnl = C_IO(tos[0]);

  return rtnStatus(p, flushFile(chnl->io), "flushing problem");
}

ReturnStatus g__flushall(processPo p, ptrPo tos) {
  flushOut();
  ReturnStatus ret = {.ret=Ok, .rslt=voidEnum};
  return ret;
}
