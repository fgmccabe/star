//
// Created by Francis McCabe on 3/11/18.
//

#ifndef STAR_THRP_H
#define STAR_THRP_H

#include "thr.h"
#include "heap.h"
#include "code.h"
#include "termP.h"

typedef struct _thread_record_ {
  clssPo clss;      /* == threadClass */
  processPo process;
} ThreadRec;

#define ThreadCellCount CellCount(sizeof(ThreadRec))

void initThreads(void);

extern threadPo allocateThread(heapPo H,processPo pr);

#endif //STAR_THRP_H
