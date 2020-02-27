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

labelPo declareLbl(const char *name, integer arity);
labelPo declareEnum(const char *name);
labelPo tplLabel(integer arity);
logical isTplLabel(labelPo lb);
logical isLabel(termPo t);

integer labelArity(labelPo lbl);
char *labelName(labelPo lbl);
integer fieldIndex(labelPo lbl, labelPo field);
integer fieldSize(labelPo lbl,labelPo field);
logical isRecordLabel(labelPo lbl);
logical sameLabel(labelPo l1, labelPo l2);

typedef retCode (*fieldProc)(labelPo lbl, integer offset, integer size, void *cl);
extern retCode applyFieldProc(labelPo lbl, integer ix, fieldProc p, void *cl);

labelPo objLabel(labelPo lbl, integer arity);

retCode showLbl(ioPo out, labelPo lbl, integer depth, integer prec, logical alt);
retCode showLabel(ioPo f, void *data, long depth, long precision, logical alt);

methodPo labelCode(labelPo lbl);

extern labelPo C_LBL(termPo t);



#endif //STAR_LBL_H
