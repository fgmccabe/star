//
// Created by Francis McCabe on 7/2/17.
//

#ifndef STAR_TEMPLATE_H
#define STAR_TEMPLATE_H

#include "hash.h"
#include "formio.h"

typedef char *(*strProc)(char *nm,void *cl);

retCode processTemplate(ioPo out, ioPo plate, hashPo vars, strProc defltPrc, void *cl);

#endif //STAR_TEMPLATE_H
