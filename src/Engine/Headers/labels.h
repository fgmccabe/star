//
// Created by Francis McCabe on 2/26/18.
//

#ifndef CAFE_LBL_H
#define CAFE_LBL_H

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "heap.h"

labelPo findLbl(char *name, integer arity);

labelPo declareLbl(char *name, integer arity);

retCode showLbl(ioPo out, labelPo lbl);

#endif //CAFE_LBL_H
