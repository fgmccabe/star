//
// Created by Francis McCabe on 3/26/25.
//

#ifndef STAR_CONSTANTSP_H
#define STAR_CONSTANTSP_H

#include "constants.h"
#include "heapP.h"

void initConstants();

void markConstants(gcSupportPo G);
retCode scanConstants(termHelper helper, void* cl);
extern ptrPo constAnts;

#endif //STAR_CONSTANTSP_H
