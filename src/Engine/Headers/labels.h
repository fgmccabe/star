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

labelPo objLabel(labelPo lbl, integer arity);

retCode showLbl(ioPo out, integer prec, logical alt, labelPo lbl);

methodPo labelCode(labelPo lbl);

extern labelPo C_LBL(termPo t);



#endif //STAR_LBL_H
