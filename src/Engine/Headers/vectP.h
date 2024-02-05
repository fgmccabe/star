//
// Created by Francis McCabe on 1/12/24.
//

#ifndef STAR_VECTP_H
#define STAR_VECTP_H

#include "vect.h"
#include "heapP.h"

extern termPo eVect;
extern labelPo lf1,lf2,lf3,lf4;
extern labelPo vct1,vct2,vct3,vct4,vectorLbl;

void initVect();
void scanVect(gcSupportPo G);

logical isVector(termPo t);

retCode dispVect(ioPo out, termPo t, integer precision, integer depth, logical alt);

typedef termPo (*makeCB)(heapPo h,integer ix,void *cl);

termPo makeVector(heapPo h,integer ln,makeCB cb,void *cl);

#endif //STAR_VECTP_H
