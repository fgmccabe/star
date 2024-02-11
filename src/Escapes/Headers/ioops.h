//
// Created by Francis McCabe on 3/5/18.
//

#ifndef STAR_IOOPS_H
#define STAR_IOOPS_H

#include "iochnnl.h"
#include "pool.h"

void initIoOps();

typedef struct asyncStruct_ *asyncPo;
typedef taskState (*nextProc)(ioPo in, asyncPo async);
typedef termPo (*asyncAlloc)(heapPo h, asyncPo async);
typedef void (*asyncClose)(ioPo io, asyncPo async);
typedef retCode (*asyncCleanup)(asyncPo async, retCode ret);

typedef struct asyncStruct_ {
  nextProc next;
  asyncAlloc alloc;
  asyncClose close;
  asyncCleanup cleanup;
  integer data;
  retCode ret;
  taskState state;
  ioPo buffer;
} AsyncStruct;

asyncPo newAsyncTask(nextProc next, asyncAlloc alloc, asyncClose close, asyncCleanup cleanup, integer data, ioPo buffer);
void asyncCloser(ioPo io, asyncPo async);

#endif //STAR_IOOPS_H
