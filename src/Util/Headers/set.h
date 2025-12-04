#ifndef _SET_H_
#define _SET_H_

#include "logical.h"
#include "retcode.h"
#include "config.h"

typedef struct set_record_ *setPo;

typedef retCode (*setElProc)(setPo s, int32 el, void *cl);

setPo createEmptySet(int32 min, int32 max, logical growable);
void deleteSet(setPo s);

retCode addToSet(setPo set, int32 k);
retCode removeFromSet(setPo set, int32 k);
logical inSet(setPo set, int32 k);

retCode processSet(setPo set, setElProc proc, void *cl);

#endif