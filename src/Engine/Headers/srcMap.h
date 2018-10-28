//
// Created by Francis McCabe on 10/21/18.
//

#ifndef STAR_SRC_MAP_H
#define STAR_SRC_MAP_H

#include "pkg.h"
#include "labels.h"

typedef struct _map_entry_ *srcMapPo;

extern packagePo srcMapPkg(srcMapPo mp);
extern labelPo srcMapFun(srcMapPo mp);
extern integer srcMapLine(srcMapPo mp);
extern integer srcMapPc(srcMapPo mp);

extern srcMapPo findInSrcMap(packagePo pkg,integer line);

extern retCode parseSrcMap(char *text,integer length);

#endif //STAR_SRC_MAP_H
