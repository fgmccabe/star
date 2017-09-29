/*
  Memory pool management
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _POOL_H_
#define _POOL_H_

#include <sys/types.h>
#include "logical.h"

/* Data structure pool management */
typedef struct _pool_ *poolPo;

poolPo newPool(size_t elsize, int initial);
logical emptyPool(poolPo base);

void *allocPool(poolPo pool); /* allocate an element from the pool */
void freePool(poolPo pool, void *el); /* free an element back to the pool */
void verifyPool(poolPo pool);

#endif
