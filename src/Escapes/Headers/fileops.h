//
// Created by Francis McCabe on 3/7/18.
//

#ifndef STAR_FILEOPS_H
#define STAR_FILEOPS_H

#include "config.h"
#include "ooio.h"

#include "engine.h"
#include "libEscapes.h"

extern char *resolveFileName(processPo p, const char *fn, integer fnLen, char *buff, integer buffLen);

ioEncoding pickEncoding(integer k);
#endif //STAR_FILEOPS_H
