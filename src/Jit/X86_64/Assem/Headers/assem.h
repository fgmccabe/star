//
// Created F.G. McCabe 10/1/2025.
//

#ifndef STAR_ASSEM_H
#define STAR_ASSEM_H

#include "config.h"
#include "x86_64.h"

codeLblPo newLabel(assemCtxPo ctx);
codeLblPo here_(assemCtxPo ctx);
#define here() here_(ctx)
codeLblPo defineLabel(assemCtxPo ctx, integer pc);

codeLblPo setLabel_(assemCtxPo ctx, codeLblPo lbl);
#define bind(lbl) setLabel_(ctx,lbl)

logical isLabelDefined(codeLblPo lbl);
uint64 labelTgt(codeLblPo lbl);
retCode cleanupLabels(assemCtxPo ctx);

#endif //STAR_ASSEM_H
