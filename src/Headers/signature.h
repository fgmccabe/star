/*
 * Specification of type signatures
 * Contact: Francis McCabe <frankmccabe@mac.com>
 */
#ifndef _SIGNATURE_H_
#define _SIGNATURE_H_

#include <logical.h>
#include <retcode.h>
#include "config.h"
#include "io.h"

/* Type signatures for Star types */
typedef enum {
  anySig = '_',  // underscore type
  voidSig = 'v', // Void type
  thisSig = 'h', // this type is a special type
  intSig = 'i', /* An integer */
  fltSig = 'f', /* A float */
  logSig = 'l', /* Logical value */
  chrSig = 's', // A string
  kvrSig = 'k', /* quantified type variable */
  kfnSig = 'K', /* quantified type function */
  tpeSig = 't',  /* A named type */
  refSig = 'r', /* A reference type */
  tpfnSig = 'z', /* A type function */

  lstSig = 'L', // Cons list
  tpeExpSig = 'U',                       /* polymorphic user type */

  tplSig = '(', /* Tuple - followed by element types followed by ) */
  faceSig = 'I', /* interface type specification */

  allSig = ':', /* universally quantified formula */
  xstSig = 'E', /* existentially quantified formula */

  funDep = 'd', // Function dependency

  constrainedSig = '|',                /* A constrained type */

  funSig = 'F', /* Function signature */
  conSig = 'C', /* constructor */
  contSig = 'D', //  continuation signature

  tpruleSig = 'Y',  /* Type rule -- aka type alias or existential type assignment */
  tplambdaSig = 'Z', /* Type function */
} starTypeSig;

typedef enum {
  univCon = ':',
  contractCon = 'c',
  implementsCon = 'a',
  constrainedCon = '|'
} constraintSig;

// Data value signatures
typedef enum {
  vodTrm = 'v',         // Void value
  intTrm = 'x',         // Integer value
  fltTrm = 'd',         // Floating point value
  strTrm = 's',         // String value
  dtaTrm = 'n',         // Constructor data value
  lstTrm = 'l',
  lblTrm = 'o',         // A structure label identifier
  enuTrm = 'e',         // An enumerated symbol
} starDecodeKey;

logical validSignature(char *s);
retCode funSigArity(char *sig, integer *arity);
retCode tupleArity(char *sig, integer *arity);
retCode skipSig(char *sig, integer *start, integer end);
retCode showSignature(ioPo out, char *sig, integer *start, integer end);
retCode showConstraint(ioPo out, char *sig, integer *start, integer end);
retCode showSig(ioPo out, char *sig);
retCode skipSignature(ioPo in);
#endif
