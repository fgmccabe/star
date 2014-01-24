#ifndef _LIST_P_H_
#define _LIST_P_H_

#include "list.h"

typedef struct _list_record_ {
  void *head;
  listPo tail;
} ListRecord;

#endif
