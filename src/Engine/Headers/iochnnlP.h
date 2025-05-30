//
// Created by Francis McCabe on 3/4/18.
//

#ifndef STAR_IO_CHNNLP_H
#define STAR_IO_CHNNLP_H

#include "iochnnl.h"
#include "code.h"
#include "termP.h"
#include "heap.h"

typedef struct io_record_ {
  ClassRecord clss;              // == ioChnnlClass
  ioPo io;
} ChnnlRecord;

#define IOChnnlCellCount CellCount(sizeof(ChnnlRecord))

extern void initIoChnnl();
extern ioChnnlPo allocateIOChnnl(heapPo H, ioPo io);

#endif //STAR_IOP_H
