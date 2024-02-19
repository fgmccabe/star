//
// Created by Francis McCabe on 3/4/18.
//

#ifndef STAR_IO_CHNNL_H
#define STAR_IO_CHNNL_H

#include "term.h"
#include "heap.h"

typedef struct io_record_ *ioChnnlPo;

extern clssPo ioChnnlClass;

extern ioChnnlPo C_IO(termPo t);
extern logical isIoChannel(termPo t);

extern ioPo ioChannel(ioChnnlPo chnnl);

extern ioChnnlPo stdInChnl(heapPo h);
extern ioChnnlPo stdOutChnl(heapPo h);
extern ioChnnlPo stdErrChnl(heapPo h);

#endif //STAR_IO_CHNNL_H
