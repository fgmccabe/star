#ifndef _ADDRESS_H_
#define _ADDRESS_H_

#include "compiler.h"

typedef struct _address_context_ *contextPo;
typedef struct _section_context_ *sectionPo;
typedef struct _label_ *labelPo;
typedef struct _address_ *addressPo;

extern sectionPo newSection(char *name);

extern labelPo defineLabel(locationPo loc,contextPo cxt, char *name);
extern labelPo findLabel(contextPo cxt,char *name);
extern long allocateBlock(contextPo cxt,long size);
extern long alignBlock(contextPo cxt,int alignment);

#endif
