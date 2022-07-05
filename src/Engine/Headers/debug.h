//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"
#include "code.h"

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

integer pcCount;     /* How many instructions executed so far? */
void dumpStats();

void showMethodCode(ioPo out, char *msg, char *name, methodPo mtd);

integer displayDepth;

retCode setupDebugChannels();

#endif //STAR_DEBUG_H
