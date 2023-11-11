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
  integer arity;
} LabelRecord, *labelRecordPo;

typedef struct program_label_ {
  clssPo clss;                // == labelClass
  LabelRecord lbl;            // The label itself
  integer index;              // Index of label in type
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  integer len;                // How long is the label name
  logical breakPointSet;      // Has a breakpoint been set for this label
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

extern clssPo labelClass;

extern void initLbls();
void markLabels(gcSupportPo G);
void showAllLabels();

void showMtdCounts(ioPo out);

methodPo labelCode(labelPo lbl);
logical labelDefined(labelPo lbl);

termPo declareEnum(const char *name, integer index, heapPo H);

#endif //STAR_LABELSP_H
