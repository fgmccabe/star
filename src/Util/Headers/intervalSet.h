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

intervalSetPo newISet();
void deleteISet(intervalSetPo s);

retCode addToISet(intervalSetPo set, int32 k);
retCode removeFromISet(intervalSetPo set, int32 k);
logical inISet(intervalSetPo set, int32 k);
logical isEmptyISet(intervalSetPo set);

retCode processISet(intervalSetPo set, elProc proc, void *cl);

retCode showISet(ioPo out,intervalSetPo set);

#endif //STAR_INTERVALSET_H
