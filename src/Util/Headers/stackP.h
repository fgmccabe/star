#ifndef _STACK_P_H_
#define _STACK_P_H_

#include "stack.h"

typedef struct _stack_record_ {
  void **data;
  int top;
  int size;
} StackRecord;

#endif
