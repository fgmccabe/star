#ifndef _STACK_H_
#define _STACK_H_

typedef struct _stack_record_ *stackPo;
typedef retCode (*stackFun)(void *datum,void *cl);

extern stackPo newStack();
extern void delStack(stackPo stack);
extern void pushStack(stackPo stack,void *data);
extern void *popStack(stackPo stack);
extern int markStack(stackPo stack);
extern void resetStack(stackPo stack,int mark);
extern retCode processStack(stackPo stack,stackFun fun,void *cl);

#endif
