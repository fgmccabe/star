//
// Created by Francis McCabe on 5/18/25.
//

#ifndef STAR_DISASS_H
#define STAR_DISASS_H

#include "ooio.h"
#include "arm64.h"

retCode disassemble(ioPo out, assemCtxPo ctx, uint32 word);

#endif //STAR_DISASS_H
