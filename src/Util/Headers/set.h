#ifndef _SET_H_
#define _SET_H_

#include "logical.h"
#include "retcode.h"
#include "config.h"
#include "ooio.h"

typedef struct set_record_ *setPo;

typedef retCode (*setElProc)(setPo s, int32 el, void *cl);

setPo createSet(int32 min);
void deleteSet(setPo s);

retCode addToSet(setPo set, int32 k);
retCode removeFromSet(setPo set, int32 k);
logical inSet(setPo set, int32 k);
logical setIsEmpty(setPo set);

retCode processSet(setPo set, setElProc proc, void *cl);

retCode showSet(ioPo out,setPo set);

#endif

