//
// Created by Francis McCabe on 2/26/18.
//

#ifndef CAFE_LBL_H
#define CAFE_LBL_H

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "heap.h"
#include "code.h"

labelPo findLbl(const char *name, integer arity);

labelPo declareLbl(const char *name, integer arity);
labelPo declareEnum(const char *name);
labelPo tplLabel(integer arity);

retCode showLbl(ioPo out, labelPo lbl);

methodPo labelCode(labelPo lbl);

#endif //CAFE_LBL_H
