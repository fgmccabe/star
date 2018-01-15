//
// Created by Francis McCabe on 1/2/18.
//

#ifndef CAFE_PP_H
#define CAFE_PP_H

#include "retcode.h"

typedef struct _display_ *ppDisplayPo;
typedef struct _disp_policy_ *policyPo;

extern retCode ppAppend(ppDisplayPo disp,policyPo pol,char *l);
extern retCode ppAppendId(ppDisplayPo disp,policyPo pol,char *l);

#endif //CAFE_PP_H
