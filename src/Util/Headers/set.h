#ifndef _SET_H_
#define _SET_H_

#include "logical.h"
#include "retcode.h"
#include "config.h"
#include "ooio.h"

typedef struct set_record_ *setPo;

typedef retCode (*setElProc)(setPo s, int32 el, void *cl);

setPo newSet();
void deleteSet(setPo s);

retCode addToSet(setPo set, int32 k);
retCode removeFromSet(setPo set, int32 k);
logical inSet(setPo set, int32 k);
logical inSetRange(setPo set, int32 from, int32 to);
logical setIsEmpty(setPo set);

setPo duplicateSet(setPo set);
setPo unionSet(setPo lhs,setPo rhs);
setPo intersectSet(setPo lhs,setPo rhs);
setPo differenceSet(setPo lhs,setPo rhs);

logical equalSets(setPo lhs,setPo rhs);

retCode processSet(setPo set, setElProc proc, void *cl);

retCode showSet(ioPo out,setPo set);

#endif

