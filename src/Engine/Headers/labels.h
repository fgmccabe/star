//
// Created by Francis McCabe on 2/26/18.
//

#ifndef STAR_LBL_H
#define STAR_LBL_H

#include "config.h"
#include "ooio.h"
#include "term.h"

typedef struct program_label_ *labelPo;

labelPo findLbl(const char *name, int32 arity);
integer labelHash(labelPo lbl);

labelPo declareLbl(const char *name, int32 arity, int32 index);
labelPo tplLabel(int32 arity);
logical isTplLabel(const char *nm) ;
logical isALabel(termPo t);

int32 labelArity(labelPo lbl);
const char * labelName(labelPo lbl);
int32 labelIndex(labelPo lbl);

logical sameLabel(labelPo l1, labelPo l2);

logical breakPointSet(labelPo lbl);
logical setBreakPoint(labelPo lbl,logical set);

integer setLabelBreakPoint(char *srch, integer slen, integer arity);
integer clearLabelBreakPoint(char *srch, integer slen, integer arity);
retCode showLabelBreakPoints(ioPo out);

typedef retCode (*labelProc)(labelPo lbl, void *cl);
retCode iterateLabels(labelProc proc,void *cl);

retCode showLbl(ioPo out, labelPo lbl, integer depth, integer prec, logical alt);
retCode showLabel(ioPo f, void *data, long depth, long precision, logical alt);

labelPo C_LBL(termPo t);

#endif //STAR_LBL_H
