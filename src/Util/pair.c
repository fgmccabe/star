/*
  Tuple pairs
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "pairP.h"
#include "utils.h"
#include <assert.h>

static integer pairHash(objectPo o);

static logical pairEquality(objectPo o1, objectPo o2);

static void pairInit(objectPo o, va_list *args);

static void destroyPair(objectPo o);

PairClassRec PairClass = {
  {
    (classPo) &ObjectClass,
    "pair",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    destroyPair,
    O_INHERIT_DEF,
    pairInit,
    sizeof(PairRecord),
    pairHash,
    pairEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo pairClass = (classPo) &PairClass;

void pairInit(objectPo o, va_list *args) {
  pairPo l = O_PAIR(o);
  l->pair.lhs = va_arg(*args, objectPo);
  l->pair.rhs = va_arg(*args, objectPo);
}

void destroyPair(objectPo o) {
  pairPo l = O_PAIR(o);

  decReference(l->pair.lhs);
  decReference(l->pair.lhs);
}

static integer pairHash(objectPo o) {
  pairPo l = O_PAIR(o);

  return hashCode(lhs(l)) * 37 + hashCode(rhs(l));
}

static logical pairEquality(objectPo o1, objectPo o2) {
  pairPo p1 = O_PAIR(o1);
  pairPo p2 = O_PAIR(o2);

  return (logical) (equals(lhs(p1), lhs(p2)) && equals(rhs(p1), rhs(p2)));
}

objectPo lhs(pairPo p) {
  return p->pair.lhs;
}

objectPo rhs(pairPo p) {
  return p->pair.rhs;
}

pairPo pair(objectPo lhs,objectPo rhs)
{
  return O_PAIR(newObject(pairClass,lhs,rhs));
}

