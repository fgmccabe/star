//
// Created by Francis McCabe on 3/11/18.
//

#ifndef STAR_THR_H
#define STAR_THR_H

#include "term.h"
#include "heap.h"
#include "engine.h"

typedef struct _thread_record_ *threadPo;

extern clssPo threadClass;    /* threadClass is a specialClass */

extern threadPo C_THREAD(termPo t);

void initThr(void);

extern threadPo threadVal(termPo t);

processPo getThreadProcess(threadPo t);

void clearProcess(threadPo t);

threadPo newThread(processPo p);

#endif //STAR_THR_H
