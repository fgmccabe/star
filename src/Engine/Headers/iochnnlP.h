//
// Created by Francis McCabe on 3/4/18.
//

#ifndef CAFE_IO_CHNNLP_H
#define CAFE_IO_CHNNLP_H

#include "iochnnl.h"
#include "code.h"
#include "termP.h"
#include "heap.h"

typedef struct _io_record_ {
  clssPo clss;                  // == integerClass
  ioPo io;
} ChnnlRecord;

#define IOChnnlCellCount CellCount(sizeof(ChnnlRecord))

extern ioChnnlPo allocateIOChnnl(heapPo H, ioPo io);

extern ioPo ioChannel(ioChnnlPo chnnl);

#endif //CAFE_IOP_H
