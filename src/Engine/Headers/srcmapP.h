//
// Created by Francis McCabe on 10/21/18.
//

#ifndef STAR_SRCMAPP_H
#define STAR_SRCMAPP_H

#include "srcmap.h"

typedef struct _map_entry_ {
  packagePo pkg;
  labelPo fun;
  integer line;
  integer pcOffset;
} SrcMapEntry;


#endif //STAR_SRCMAPP_H
