#ifndef _RUNTIME_H_
#define _RUNTIME_H_

typedef void* genericPo;
typedef struct _environment_ *environmentPo;
typedef struct _frame_ *framePo;

typedef void (*continuationPo)(framePo fp,environmentPo env);

typedef void (*caseFun)(genericPo data,continuationPo *cases);


#endif
