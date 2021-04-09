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


typedef struct field_bucket_ {
  labelPo lbl;
  integer offset; // offset in bytes of field
} FieldBucket, *fieldPo;

typedef struct field_table_ {
  integer size;
  struct field_bucket_ entries[ZEROARRAYSIZE];
} FieldTable, *fieldTblPo;

typedef struct labelTable_ *lblTablePo;

typedef struct program_label_ {
  clssPo clss;                // == labelClass
  integer arity;              // Arity of label
  integer index;              // Index of label in type
  integer hash;               // Hash code for the label
  methodPo mtd;               // Optimization - is a method defined for this label?
  char *name;                 // LblRecord name
  integer len;                // How long is the label name
  fieldTblPo fields;          // Spec of the fields of this term
  lblTablePo table;           // table of similar labels (different arities)
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
