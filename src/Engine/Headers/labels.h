//
// Created by Francis McCabe on 2/26/18.
//

#ifndef STAR_LBL_H
#define STAR_LBL_H

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "heap.h"
#include "code.h"

labelPo findLbl(const char *name, integer arity);
integer labelHash(labelPo lbl);

labelPo declareLbl(const char *name, integer arity, integer index);
termPo declareEnum(const char *name, integer index, heapPo H);
labelPo otherLbl(labelPo lbl, integer arity);
labelPo tplLabel(integer arity);
logical isTplLabel(const char *nm) ;
logical isALabel(termPo t);

integer labelArity(labelPo lbl);
char *labelName(labelPo lbl);
integer labelIndex(labelPo lbl);

logical isLabel(labelPo lbl, char *nm, integer arity);
logical sameLabel(labelPo l1, labelPo l2);

logical breakPointSet(labelPo lbl);
logical setBreakPoint(labelPo lbl,logical set);

integer setLabelBreakPoint(char *srch, integer slen, integer arity);
integer clearLabelBreakPoint(char *srch, integer slen, integer arity);
retCode showLabelBreakPoints(ioPo out);

typedef retCode (*labelProc)(labelPo lbl, void *cl);
retCode iterateLabels(labelProc proc,void *cl);

labelPo objLabel(labelPo lbl, integer arity);

retCode showLbl(ioPo out, labelPo lbl, integer depth, integer prec, logical alt);
retCode showLabel(ioPo f, void *data, long depth, long precision, logical alt);

methodPo labelCode(labelPo lbl);

labelPo C_LBL(termPo t);

logical isLabelPo(termPo t);

#endif //STAR_LBL_H
