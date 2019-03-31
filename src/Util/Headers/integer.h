/*
  Definition of integer
  Copyright (c) 2016, 2017, 2018. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _INTEGER_H_
#define _INTEGER_H_

#include "config.h"

typedef int64 integer;
typedef unsigned char byte;
typedef uint64 uinteger;

#include "object.h"

#define LARGE_INT64 ((integer)(((uinteger)-1)>>1))
#define LARGE_INT32 ((int32)(((uint32)-1)>>1))
#define SMALL_INT64 ((integer) 0x8000000000000000)

#define INT32_DIGITS (10)
#define INT64_DIGITS (19)

typedef struct _ix_object_ *ixPo;

extern integer ixVal(ixPo i);
extern ixPo newInteger(integer ix);

extern classPo ixClass;

#ifdef VERIFY_OBJECT
#define O_IX(c) ((ixPo)(checkCast((c),ixClass)))
#else
#define O_IX(c) ((ixPo)(c))
#endif

#endif
