//
// Created F.G. McCabe 9/30/2025.
//

#include "config.h"
#include "arm64.h"

codeLblPo newLabel(assemCtxPo ctx);
codeLblPo here_(assemCtxPo ctx);
#define here() here_(ctx)
codeLblPo defineLabel(assemCtxPo ctx, integer pc);

codeLblPo setLabel_(assemCtxPo ctx, codeLblPo lbl);
#define bind(lbl) setLabel_(ctx,lbl)

logical isLabelDefined(codeLblPo lbl);
uint64 labelTgt(codeLblPo lbl);
retCode cleanupLabels(assemCtxPo ctx);
