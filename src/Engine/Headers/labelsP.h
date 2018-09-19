//
// Created by Francis McCabe on 2/27/18.
//

#ifndef STAR_LABELSP_H
#define STAR_LABELSP_H

#include "labels.h"
#include "heapP.h"

/*
 * A program label structure
 */

typedef struct _program_label_ {
  clssPo clss;                // == labelClass
  integer arity;              // Arity of label
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  char *name;                 // LblRecord name
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

extern void initLbls();
void markLabels(gcSupportPo G);

#endif //STAR_LABELSP_H
