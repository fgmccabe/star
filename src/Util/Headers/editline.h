//
// Created by Francis McCabe on 2018-12-22.
//

#ifndef STAR_EDITLINE_H
#define STAR_EDITLINE_H

#include "integer.h"
#include "retcode.h"
#include "vector.h"
#include "stringBuffer.h"


typedef retCode (*CompletionCallback)(bufferPo lineBuff,void *cl,integer ix);

void setEditLineCompletionCallback(CompletionCallback fn, void *cl);

void clearEditLineCompletionCallback();


retCode consoleInput(bufferPo lineBuff);

void saveHistory();
retCode initHistory(char *filename);

#endif //STAR_EDITLINE_H
