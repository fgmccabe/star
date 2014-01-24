#ifndef _METHOD_P_H_
#define _METHOD_P_H_

#include "method.h"
#include "list.h"

typedef struct _gc_scan_block_ *gcScanPo;
typedef struct _gc_scan_block_ {
  lPo callSite;
  lPo scanCode;
  listPo references;			/* List of referenced variables */
  gcScanPo next;
} GcScanBlock;

typedef enum {
  integerLiteral,
  floatLiteral,
  stringLiteral,
  otherLiteral } literalType;

typedef struct _literal_ *literalPo;
typedef struct _literal_ {
  literalType type;
  union {
    integer i;
    double d;
    uniChar *str;
    struct {
      void *add;
      long size;
    } other;
  } lit;
  lPo lbl;
  literalPo next;
} LiteralRecord;

typedef struct _method_context_ {
  uniChar *defName;			/* Name of this definition */
  listPo literals;
  tryPo tryBlocks;
  gcScanPo scanBlocks;
  assemPo code;				/* What package are we in? */
  cafeFun generated;			/* Generated code */
} Method;

typedef struct _try_block_ {
  lPo from;				/* The range of the catch block */
  lPo to;
  lPo catchCode;
  tryPo next;
} TryBlock;




#endif
