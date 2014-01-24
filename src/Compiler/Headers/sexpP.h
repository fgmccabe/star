#ifndef _SEXP_P_H_
#define _SEXP_P_H_
#include "sexp.h"
#include "pp.h"

typedef enum {
  applyCon,				/* Constructor for apply terms */
  intCon,				/* Constructor for 32-bit integers */
  integerCon,				/* Constructor for 64-bit integers */
  floatCon,				/* Constructor for floating points */
  stringCon,				/* Constructor for strings */
  charCon,				/* Constructor for char literals */
  symbCon				/* Constructor for symbols */
} SexpType;

typedef struct {
  sxPo op;
  lxPo args;
} ApplyPair;

typedef struct _sequence_object_ {
  sxPo head;
  lxPo tail;
} SequenceObj;

typedef struct _sexp_object_ {
  SexpType con;
  locationPo loc;			/* Location object of this s-exp */
  union{
    ApplyPair app;			/* Op and Args */
    uniChar *text;			/* Text of string or identifier */
    double d;				/* A floating point number */
    integer ix;				/* An integer */
    uniChar ch;
  } S;
} SexpObject;


extern retCode dispSexp(ppDisplayPo disp, policyPo pol, sxPo sx);
extern retCode dispSeq(ppDisplayPo disp, policyPo pol, lxPo lx, char *pre, char *sep, char *post);

#endif
