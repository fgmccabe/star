//
// Created by Francis McCabe on 7/10/20.
//

#ifndef STAR_ARM64P_H
#define STAR_ARM64P_H

#include "jitP.h"
#include "arm64.h"
#include "ooio.h"
#include "array.h"

void clearCodeCtxMaps(assemCtxPo ctx);


codeLblPo preamble(assemCtxPo ctx, int32 lclCount);
retCode postamble(assemCtxPo ctx);

#endif //STAR_ARM64P_H
