//
// Created by Francis McCabe on 3/7/18.
//

#ifndef CAFE_FILEOPS_H
#define CAFE_FILEOPS_H

#include "config.h"
#include "ooio.h"

#include "engine.h"
#include "libEscapes.h"

extern char *resolveFileName(processPo p, const char *fn, integer fnLen, char *buff, integer buffLen);
#endif //CAFE_FILEOPS_H
