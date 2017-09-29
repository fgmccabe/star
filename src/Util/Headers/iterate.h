#ifndef _ITERATE_H_
#define _ITERATE_H_
/*
  Iteration header
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "logical.h"

typedef logical (*hasNext)(void *cl);	/* does iterator have a next element? */
typedef void* (*nextFun)(void *cl);	// next element function

typedef struct {
  hasNext hasNext;
  nextFun next;
} Iterator;

#endif
