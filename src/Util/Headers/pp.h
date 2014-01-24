#ifndef _PP_H_
#define _PP_H_

#include <io.h>

typedef struct _display_policy_ {
  int indent;
} DisplayPolicy, *policyPo;

typedef struct _display_ {
  ioPo file;
  int currColumn;
  retCode ret;
} PPDisplay, *ppDisplayPo;

typedef retCode (*ppDisplay)(ppDisplayPo disp,policyPo policy,void *data);

extern retCode ppAppend(ppDisplayPo disp,policyPo policy,char *str);
extern retCode ppAppendId(ppDisplayPo disp,policyPo pol,uniChar *str);
extern retCode ppAppendC(ppDisplayPo disp,policyPo pol,uniChar ch);
extern retCode ppAppendU(ppDisplayPo disp,policyPo policy,uniChar *str);
extern retCode ppAppendQ(ppDisplayPo disp,policyPo pol,uniChar *str);
extern retCode ppAppendI(ppDisplayPo disp,policyPo policy,integer ix);
extern retCode ppAppendF(ppDisplayPo disp,policyPo policy,double d);

extern PPDisplay initDisplay(ioPo f);

#endif
