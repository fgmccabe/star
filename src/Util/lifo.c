//
// Created by Francis McCabe on 3/21/25.
//
#include <assert.h>
#include "lifoP.h"
#include "pool.h"
#include "utils.h"

static poolPo elementPool = Null;

void initLifo(){
  elementPool = newPool(sizeof(LifoElementRec),64);
}

lifoPo pushElement(void *el,lifoPo stk){
  lifoPo new = (lifoPo) allocPool(elementPool);
  new->el = el;
  new->prev = stk;
  return new;
}

lifoPo popElement(void **top, lifoPo stk){
  assert(stk!=Null);
  *top = stk->el;
  lifoPo prev = stk->prev;
  freePool(elementPool,stk);
  return prev;
}

void *peekElement(lifoPo stk, int32 ix){
  while(ix>0){
    assert(stk!=Null);
    stk = stk->prev;
  }
  return stk->el;
}
