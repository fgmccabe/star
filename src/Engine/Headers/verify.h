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


retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen);

extern classPo segmentClass;


#endif //STAR_VERIFY_H
