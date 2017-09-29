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
  voidSig = 'v', // Void type
  thisSig = 'h', // this type is a special type
  intSig = 'i', /* An integer */
  fltSig = 'f', /* A float */
  logSig = 'l', /* Logical value */
  strSig = 'S', /* String */
  kvrSig = 'k', /* quantified type variable */
  kfnSig = 'K', /* quantified type function */
  tpeSig = 't',  /* A named type */
  tpfnSig = 'z', /* A type function */

  lstSig = 'L', /* List */
  tpeExpSig = 'U',                       /* polymorphic user type */

  tplSig = '(', /* Tuple - followed by element types followed by ) */
  faceSig = 'I', /* interface type specification */

  allSig = ':', /* universally quantified formula */
  xstSig = 'E', /* existentially quantified formula */

  constrainedSig = '|',                /* A constrained type */

  funSig = 'F', /* Function signature */
  conSig = 'C', /* constructor */

  tpruleSig = 'Y',  /* Type rule -- aka type alias or existential type assignment */
  tplambdaSig = 'Z', /* Type function */
} cafeTypeSig;

typedef enum {
  univCon = ':',
  contractCon = 'c',
  implementsCon = 'a',
  constrainedCon = '|'
} constraintSig;

// Data value signatures
typedef enum {
  intTrm = 'x',        // Integer value
  fltTrm = 'd',        // Floating point value
  strTrm = 's',        // String value
  enuTrm = 'o',        // Symbol name
  dtaTrm = 'n',        // Constructor data value
} cafeTermSig;

/* First Star version */
// First four bytes of any code sequence must be this magic number
#define SIGNATURE 0x01030507L  /* code signature */
#define SIGNBSWAP 0x03010705L  /* signature when we must swap bytes not words */
#define SIGNWSWAP 0x05070103L  /* signature to sap words only */
#define SIGNBWSWP 0x07050301L  /* when we have to swap words and bytes */

extern logical validSignature(char *s);
extern retCode functionArity(char *sig, integer *arity);
extern retCode tupleArity(char *sig, integer *arity);
extern retCode skipSig(char *sig, integer *start, integer end);
extern retCode skipConstraint(char *sig, integer *start, integer end);
extern retCode showSignature(ioPo out, char *sig, integer *start, integer end);
extern retCode showConstraint(ioPo out, char *sig, integer *start, integer end);
extern retCode showSig(ioPo out, char *sig);
#endif

