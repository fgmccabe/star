//
// Created by Francis McCabe on 2/26/18.
//

#ifndef STAR_LBL_H
#define STAR_LBL_H

#include "config.h"
#include "ooio.h"

typedef struct program_label_* labelPo;

typedef struct term_record {
  int32 lblIndex;
  int32 space;
} TermHead;

labelPo findLbl(const char* name, int32 arity);
uint64 labelHash(labelPo lbl);

labelPo declareLbl(const char* name, int32 arity, int32 constructorIndex);
labelPo tplLbl(int32 arity);
logical isTplLabel(const char* nm);

const char* lblName(labelPo lbl);
int32 constructorIndex(labelPo lbl);
labelPo indexToLabel(int32 index);

logical sameLabel(labelPo l1, labelPo l2);

logical breakPointSet(labelPo lbl);
logical setBreakPoint(labelPo lbl, logical set);

integer setLabelBreakPoint(char* srch, integer slen, integer arity);
integer clearLabelBreakPoint(char* srch, integer slen, integer arity);
retCode showLabelBreakPoints(ioPo out);

typedef retCode (*labelProc)(labelPo lbl, void* cl);
retCode iterateLabels(labelProc proc, void* cl);

retCode showLbl(ioPo out, labelPo lbl, integer depth, integer prec, logical alt);
retCode showLabel(ioPo f, void* data, long depth, long precision, logical alt);

#ifndef NDEBUG
#define C_LBL(c) ((labelPo)(checkIndex((c),labelIndex)))
#else
#define C_LBL(t) ((labelPo) (t))
#endif

#endif //STAR_LBL_H
