/*
 * implementation of list structure
 */
#include "config.h"
#include "listP.h"
#include "utils.h"
#include "pool.h"
#include <assert.h>

listPo emptyList=Null;

static poolPo listPool=Null;

static void initLists()
{
  if(listPool==Null){
    listPool = newPool(sizeof(ListRecord),256);
  }
}

void *head(listPo list)
{
  return list->head;
}

listPo tail(listPo list)
{
  return list->tail;
}

listPo cons(void *head,listPo tail)
{
  initLists();

  listPo c = (listPo)allocPool(listPool);

  c->head = head;
  c->tail = tail;
  return c;
}

listPo tack(void *head,listPo list)
{
  if(list==emptyList)
    return cons(head,list);
  else{
    listPo l = list;
    while(l->tail!=emptyList)
      l = l->tail;
    l->tail = cons(head,emptyList);
    return list;
  }
}

void* listNthElement(listPo list,int32 ix)
{
  while(list!=emptyList && ix-->0)
    list = tail(list);
  if(list==emptyList)
    return Null;
  else
    return head(list);
}

void releaseList(listPo list)
{
  while(list!=emptyList){
    listPo next = tail(list);
    freePool(listPool,list);
    list = next;
  }
}

retCode processList(listPo list,listFun fun,void *cl)
{
  retCode ret = Ok;
  while(ret==Ok && list!=emptyList){
    ret = fun(head(list),cl);
    list = tail(list);
  }
  return ret;
}

void* findInList(listPo list,listTest test,void *cl)
{
  while(list!=emptyList){
    if(test(head(list),cl))
      return head(list);
    list = tail(list);
  }
  return Null;
}

long listCount(listPo list)
{
  long count = 0;
  while(list!=emptyList){
    count++;
    list = tail(list);
  }
  return count;
}

logical sameLists(listPo l1,listPo l2)
{
  while(l1!=emptyList && l2!=emptyList){
    if(head(l1)!=head(l2))
      return False;
    l1 = tail(l1);
    l2 = tail(l2);
  }
  return l1==emptyList && l2==emptyList;
}

listPo removeElements(listPo l,listTest test,void *cl)
{
  listPo l1 = l;
  listPo tl = emptyList;
  while(l1!=emptyList){
    if(test(head(l1),cl)){
      if(tl==emptyList){		/* top of list */
	assert(l1==l);
	l1 = l = tail(l);
      }
      else{
	l1 = tail(l1);
	tl->tail = l1;			/* chop out l1 */
      }
    }
    else{
      tl = l1;
      l1 = tail(l1);
    }
  }
  return l;
}
