#ifndef _ADDRESS_P_H_
#define _ADDRESS_P_H_

#include "address.h"
#include "hash.h"

typedef struct _address_context_ {
  long currentNext;
  long size; 
  uniChar *name;
  hashPo entries;
} ContextRecord;

typedef struct _label_ {
  locationPo loc;
  uniChar *name;
  long offset;
  contextPo cxt;
} LabelRecord;

/*
 * There are several forms of address, from a simple label to a combination of address expressions
 */

typedef enum {
  label, labelDiff, addressOffset
} addressExpType;

typedef struct _address_ {
  addressExpType type;
  labelPo label;
  union {
    labelPo diff;
    long offset;
  } addr;
} AddressRecord;

#define DEFAULT_TABLE_SIZE 128

#endif
