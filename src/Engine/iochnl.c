//
// Created by Francis McCabe on 3/4/18.
//

#include "iochnnlP.h"
#include "assert.h"
#include "labelsP.h"

static long ioSize(builtinClassPo cl, termPo o);
static termPo ioCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo ioScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical ioCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer ioHash(builtinClassPo cl, termPo o);
static retCode ioDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo ioFinalizer(builtinClassPo class, termPo o);

BuiltinTerm IOChnnlClass = {
  .special = {0,0},
  .sizeFun = ioSize,
  .copyFun = ioCopy,
  .scanFun = ioScan,
  .finalizer = ioFinalizer,
  .compFun = ioCmp,
  .hashFun = ioHash,
  .dispFun = ioDisp
};

builtinClassPo ioChnnlClass = &IOChnnlClass;
int32 iochnlIndex;

void initIoChnnl() {
  IOChnnlClass.special.lblIndex = specialIndex;
  iochnlIndex = standardIndex(ioChnnlClass);
}

ioChnnlPo allocateIOChnnl(heapPo H, ioPo io) {
  ioChnnlPo t = (ioChnnlPo) allocateObject(H, iochnlIndex, IOChnnlCellCount);
  t->io = io;
  return t;
}

long ioSize(builtinClassPo cl, termPo o) {
  return IOChnnlCellCount;
}

termPo ioCopy(builtinClassPo cl, termPo dst, termPo src) {
  ioChnnlPo si = C_IO(src);
  ioChnnlPo di = (ioChnnlPo) (dst);
  *di = *si;
  return (termPo) di + IOChnnlCellCount;
}

termPo ioScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + IOChnnlCellCount);
}

termPo ioFinalizer(builtinClassPo class, termPo o) {
  ioChnnlPo chnl = C_IO(o);

  // close the channel
  if (chnl->io != Null)
    closeIo(chnl->io);

  return (termPo) (o + IOChnnlCellCount);
}

logical ioCmp(builtinClassPo cl, termPo o1, termPo o2) {
  ioChnnlPo i1 = C_IO(o1);
  ioChnnlPo i2 = C_IO(o2);

  return (logical) (i1->io == i2->io);
}

integer ioHash(builtinClassPo cl, termPo o) {
  ioChnnlPo io = C_IO(o);
  return hash61((integer) io->io);
}

static retCode ioDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  ioChnnlPo ch = C_IO(t);
  return outMsg(out, "<<io:%s>>", fileName(ch->io));
}

ioChnnlPo C_IO(termPo t) {
  assert(hasIndex(t, iochnlIndex));
  return (ioChnnlPo) t;
}

ioPo ioChannel(ioChnnlPo chnnl) {
  return chnnl->io;
}

retCode closeChannel(ioChnnlPo chnnl) {
  if (chnnl->io != Null) {
    retCode ret = closeIo(chnnl->io);
    chnnl->io = Null;
    return ret;
  } else
    return Ok;
}

logical isIoChannel(termPo t) {
  return hasIndex(t, iochnlIndex);
}

static ioChnnlPo inChnl = Null;
static ioChnnlPo outChnl = Null;
static ioChnnlPo errChnl = Null;

ioChnnlPo stdInChnl(heapPo h) {
  if (inChnl == Null) {
    inChnl = allocateIOChnnl(h, Stdin());
  }
  return inChnl;
}

ioChnnlPo stdOutChnl(heapPo h) {
  if (outChnl == Null) {
    outChnl = allocateIOChnnl(h, Stdout());
  }
  return outChnl;
}

ioChnnlPo stdErrChnl(heapPo h) {
  if (errChnl == Null) {
    errChnl = allocateIOChnnl(h, Stderr());
  }
  return errChnl;
}
