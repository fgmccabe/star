#include "config.h"
#include <ooio.h>
#include "stackP.h"
#include "utils.h"
#include <stdlib.h>
#include <assert.h>


static poolPo stackPool = Null;

void initStack()
{
  if(stackPool==Null){
    stackPool = newPool(sizeof(StackRecord),16);
  }
}

stackPo newStack()
{
  initStack();
  stackPo stk = (stackPo)allocPool(stackPool);
  stk->data = malloc(sizeof(void*)*8);
  stk->top = 0;
  stk->size = 8;
  return stk;
}

void delStack(stackPo stack)
{
  free(stack->data);
  freePool(stackPool,stack);
}

void pushStack(stackPo stk,void *datum)
{
  if(stk->top==stk->size){
    int newSize = stk->size+stk->size/2;
    void **newData = malloc(sizeof(void*)*newSize);
    for(int ix=0;ix<stk->top;ix++)
      newData[ix]=stk->data[ix];
    free(stk->data);
    stk->data=newData;
    stk->size = newSize;
  }
  stk->data[stk->top++]=datum;
}

void *popStack(stackPo stk)
{
  assert(stk->top>0);
  return stk->data[--stk->top];
}

int markStack(stackPo stk)
{
  return stk->top;
}

void resetStack(stackPo stk,int mark)
{
  assert(stk->top>=mark);
  stk->top=mark;
}

retCode processStack(stackPo stk,stackFun fun,void *cl)
{
  retCode ret = Ok;
  for(int ix=stk->top-1;ret==Ok && ix>=0;ix--)
    ret = fun(stk->data[ix],cl);
  return ret;
}
