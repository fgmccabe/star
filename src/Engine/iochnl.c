//
// Created by Francis McCabe on 3/4/18.
//

#include "iochnnlP.h"
#include "assert.h"

static long ioSize(specialClassPo cl, termPo o);
static termPo ioCopy(specialClassPo cl, termPo dst, termPo src);
static termPo ioScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static retCode ioDisp(ioPo out, termPo t, long depth, logical alt);

SpecialClass IOChnnlClass = {
  .clss = Null,
  .sizeFun = ioSize,
  .copyFun = ioCopy,
  .scanFun = ioScan,
  .dispFun = ioDisp
};

clssPo ioChnnlClass = (clssPo) &IOChnnlClass;

void initIoChnnl() {
  IOChnnlClass.clss = specialClass;
}

ioChnnlPo allocateIOChnnl(heapPo H, ioPo io) {
  ioChnnlPo t = (ioChnnlPo) allocateObject(H, ioChnnlClass, IOChnnlCellCount);
  t->io = io;
  return t;
}

long ioSize(specialClassPo cl, termPo o) {
  return IOChnnlCellCount;
}

termPo ioCopy(specialClassPo cl, termPo dst, termPo src) {
  ioChnnlPo si = C_IO(src);
  ioChnnlPo di = (ioChnnlPo) (dst);
  *di = *si;
  return (termPo) di + IOChnnlCellCount;
}

termPo ioScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + IOChnnlCellCount);
}

static retCode ioDisp(ioPo out, termPo t, long depth, logical alt) {
  ioChnnlPo io = C_IO(t);
  return outMsg(out, "<<io:0x%x>>", io->io);
}

ioChnnlPo C_IO(termPo t) {
  assert(hasClass(t, ioChnnlClass));
  return (ioChnnlPo) t;
}

ioPo ioChannel(ioChnnlPo chnnl) {
  return chnnl->io;
}

static ioChnnlPo inChnl = Null;
static ioChnnlPo outChnl = Null;
static ioChnnlPo errChnl = Null;

ioChnnlPo stdInChnl(heapPo h) {
  if (inChnl == Null) {
    inChnl = allocateIOChnnl(h, stdIn);
  }
  return inChnl;
}

ioChnnlPo stdOutChnl(heapPo h) {
  if (outChnl == Null) {
    outChnl = allocateIOChnnl(h, stdOut);
  }
  return inChnl;
}

ioChnnlPo stdErrChnl(heapPo h) {
  if (errChnl == Null) {
    errChnl = allocateIOChnnl(h, stdErr);
  }
  return inChnl;
}

void scanChnnl() {

}
