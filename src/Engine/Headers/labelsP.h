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


typedef struct _field_bucket_ {
  labelPo lbl;
  integer offset; // offset in bytes of field
} FieldBucket, *fieldPo;

typedef struct _field_table_ {
  integer size;
  struct _field_bucket_ entries[ZEROARRAYSIZE];
} FieldTable, *fieldTblPo;

typedef struct _program_label_ {
  clssPo clss;                // == labelClass
  integer arity;              // Arity of label
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  char *name;                 // LblRecord name
  fieldTblPo fields;          // Spec of the fields of this term
} LblRecord;

#define LabelCellCount CellCount(sizeof(LblRecord))

extern void initLbls();
void markLabels(gcSupportPo G);

void declareFields(labelPo lbl, fieldTblPo tbl);
fieldTblPo newFieldTable(integer count);
void clearFieldTable(labelPo lbl);
void destroyFieldTable(fieldTblPo tbl);
void setFieldTblEntry(fieldTblPo tbl, labelPo field, integer offset);
#endif //STAR_LABELSP_H
