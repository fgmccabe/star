//
// Created by Francis McCabe on 3/4/18.
//

#ifndef CAFE_IO_CHNNL_H
#define CAFE_IO_CHNNL_H

#include "term.h"
#include "heap.h"

typedef struct _io_record_ *ioChnnlPo;

extern clssPo ioChnnlClass;

extern ioChnnlPo C_IO(termPo t);

extern ioPo ioChannel(ioChnnlPo chnnl);

extern ioChnnlPo stdInChnl(heapPo h);
extern ioChnnlPo stdOutChnl(heapPo h);
extern ioChnnlPo stdErrChnl(heapPo h);

#endif //CAFE_IO_CHNNL_H
