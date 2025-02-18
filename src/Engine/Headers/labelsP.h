//
// Created by Francis McCabe on 2/27/18.
//

#ifndef STAR_LABELSP_H
#define STAR_LABELSP_H

#include "labels.h"
#include "heapP.h"
#include "code.h"

/*
 * A program label structure
 */


extern integer maxLabels;

typedef struct {
  const char *name;
  int32 arity;
} LabelRecord, *labelRecordPo;

typedef struct program_label_ {
  ClassRecord clss;           // == labelClass
  LabelRecord lbl;            // The label itself
  int32 index;                // Index of label in type
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  int32 len;                  // How long is the label name
  logical breakPointSet;      // Has a breakpoint been set for this label
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

extern void initLbls();
void markLabels(gcSupportPo G);
__attribute__((unused)) void showAllLabels();

methodPo labelCode(labelPo lbl);
logical labelDefined(labelPo lbl);

termPo declareEnum(const char *name, int32 index, heapPo H);

extern integer lblTableTop;
extern LblRecord *labelTable;

#endif //STAR_LABELSP_H
