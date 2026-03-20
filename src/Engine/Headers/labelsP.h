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


extern builtinClassPo labelClass;
extern int32 labelIndex;      // THe index of labels themselves.
extern int32 specialIndex;

extern integer maxLabels;

typedef struct {
  const char *name;
  int32 arity;
} LabelRecord, *labelRecordPo;

typedef struct program_label_ {
  TermHead special;         // == labelClass
  LabelRecord lbl;            // The label itself
  int32 constructorIndex;     // Index of label in type
  int32 labelIndex;           // Convenience access to label's index
  uint64 hash;                // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  int32 len;                  // How long is the label name
  logical breakPointSet;      // Has a breakpoint been set for this label
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

extern void initLbls();
void markLabels(gcSupportPo G);
__attribute__((unused)) void showAllLabels();

int32 standardIndex(builtinClassPo clss);

logical labelDefined(labelPo lbl);

termPo declareEnum(const char *name, int32 index, heapPo H);

static inline int32 lblArity(labelPo lbl) {
  return lbl->lbl.arity;
}

static inline methodPo labelMtd(labelPo lbl) {
  return lbl->mtd;
}

int32 indexOfLabel(labelPo lbl);

#endif //STAR_LABELSP_H
