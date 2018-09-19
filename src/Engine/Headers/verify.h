//
// Created by Francis McCabe on 7/26/18.
//

#ifndef STAR_VERIFY_H
#define STAR_VERIFY_H

#include "codeP.h"
#include <assert.h>
#include "formioP.h"
#include "labels.h"
#include "engineOptions.h"

typedef struct _code_segment_ *segPo;

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen);

extern classPo segmentClass;

#ifdef VERIFY_OBJECT
#define O_SEG(c) ((segPo)(checkCast((c),segmentClass)))
#else
#define O_SEG(c) ((segPo)(c))
#endif

#endif //STAR_VERIFY_H
