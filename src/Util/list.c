/*
  Cons Lists
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"
#include "utils.h"
#include "listP.h"
#include <assert.h>

static integer listHash(objectPo o);

static logical listEquality(objectPo o1, objectPo o2);

static void listInit(objectPo o, va_list *args);

static void eraseList(objectPo o);

ConsClassRec ListClass = {
  {
    (classPo) &ObjectClass,
    "list",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    eraseList,
    listInit,
    sizeof(ConsRecord),
    listHash,
    listEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo listClass = (classPo) &ListClass;

ConsRecord EmptyList = {
  {(classPo) &ListClass,
         LARGE_INT32
  },
  {NULL, NULL}
};

listPo nilList = &EmptyList;

void listInit(objectPo o, va_list *args) {
  listPo l = O_LIST(o);
  l->list.head = va_arg(*args, objectPo);
  l->list.tail = va_arg(*args, listPo);

  incReference(l->list.head);
  incReference(O_OBJECT(l->list.tail));
}

void eraseList(objectPo o) {
  listPo l = O_LIST(o);

  decReference(l->list.head);
  decReference(O_OBJECT(l->list.tail));
}

static integer listHash(objectPo o) {
  listPo l = O_LIST(o);
  if (l == nilList)
    return 0;
  else
    return hashCode(head(l)) * 37 + listHash(O_OBJECT(tail(l)));
}

static logical listEquality(objectPo o1, objectPo o2) {
  listPo l1 = O_LIST(o1);
  listPo l2 = O_LIST(o2);

  while (l1 != nilList && l2 != nilList) {
    if (!equals(l1->list.head, l2->list.head))
      return False;
    l1 = tail(l1);
    l2 = tail(l2);
  }

  return (logical) (l1 == nilList && l2 == nilList);
}

void *head(listPo list) {
  return list->list.head;
}

listPo tail(listPo list) {
  return list->list.tail;
}

listPo cons(objectPo head, listPo tail) {
  return O_LIST(newObject(listClass, head, tail));
}

listPo tack(objectPo head, listPo list) {
  if (list == nilList)
    return cons(head, list);
  else {
    listPo l = list;
    while (l->list.tail != nilList)
      l = l->list.tail;
    l->list.tail = cons(head, nilList);
    return list;
  }
}

objectPo listNthElement(listPo list, int64 ix) {
  while (list != nilList && ix-- > 0)
    list = tail(list);
  if (list == nilList)
    return Null;
  else
    return head(list);
}

retCode processCons(listPo list, listFun fun, void *cl) {
  retCode ret = Ok;
  while (ret == Ok && list != nilList) {
    ret = fun(head(list), cl);
    list = tail(list);
  }
  return ret;
}

void *findInList(listPo list, listTest test, void *cl) {
  while (list != nilList) {
    if (test(head(list), cl))
      return head(list);
    list = tail(list);
  }
  return Null;
}

long listCount(listPo list) {
  long count = 0;
  while (list != nilList) {
    count++;
    list = tail(list);
  }
  return count;
}

listPo removeElements(listPo l, listTest test, void *cl) {
  listPo l1 = l;
  listPo tl = nilList;
  while (l1 != nilList) {
    if (test(head(l1), cl)) {
      if (tl == nilList) {    /* top of list */
        assert(l1 == l);
        l1 = l = tail(l);
      } else {
        l1 = tail(l1);
        tl->list.tail = l1;      /* chop out l1 */
      }
    } else {
      tl = l1;
      l1 = tail(l1);
    }
  }
  return l;
}

listPo filter(listPo l, listTest test, void *cl) {
  if (l == nilList)
    return l;
  else if (test(head(l), cl)) {
    return cons(head(l), filter(tail(l), test, cl));
  } else
    return filter(tail(l), test, cl);
}

void *listFold(listPo l, folder f, void *state) {
  while (l != nilList) {
    state = f(head(l), state);
    l = tail(l);
  }
  return state;
}

void releaseList(listPo l) {
  eraseList(O_OBJECT(l));
}

static void split(listPo lst, listPo *lft, listPo *rgt) {
  if (lst == nilList) {
    *lft = nilList;
    *rgt = nilList;
  } else if (tail(lst) == nilList) {
    *lft = lst;
    *rgt = nilList;
  } else {
    listPo next = tail(lst);
    split(tail(next), &lst->list.tail, &next->list.tail);
  }
}

static listPo merge(listPo lft, listPo rgt, objCompare comp, void *cl) {
  if (lft == nilList)
    return rgt;
  else if (rgt == nilList)
    return lft;
  else if (comp(head(lft), head(rgt), cl) != bigger) {
    lft->list.tail = merge(lft->list.tail, rgt, comp, cl);
    return lft;
  } else {
    rgt->list.tail = merge(lft, rgt->list.tail, comp, cl);
    return rgt;
  }
}
