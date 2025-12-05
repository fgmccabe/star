#ifndef _SETP_H_
#define _SETP_H_

#include "set.h"

typedef struct set_record_{
  int32 min;
  int32 count;
  uint64* data;
} SetRecord;


#endif
