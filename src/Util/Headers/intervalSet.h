//
// Created by Francis McCabe on 1/27/26.
//

#ifndef STAR_INTERVALSET_H
#define STAR_INTERVALSET_H

#include "config.h"
#include "ioP.h"
#include "logical.h"

typedef struct iset_ *intervalSetPo;

typedef retCode (*elProc)(int32 from, int32 to, void *cl);

intervalSetPo newIntervalSet();
void deleteIntervalSet(intervalSetPo s);

void addToIntervalSet(intervalSetPo set, int32 k);
void removeFromIntervalSet(intervalSetPo set, int32 k);
logical inIntervalSet(intervalSetPo set, int32 k);
logical intervalSetIsEmpty(intervalSetPo set);

retCode processIntervalSet(intervalSetPo set, elProc proc, void *cl);

retCode showIntervalSet(ioPo out,intervalSetPo set);

retCode firstElement(intervalSetPo set, int32 from, int32 *i);

logical checkIntervalSet(intervalSetPo set);

#endif //STAR_INTERVALSET_H
