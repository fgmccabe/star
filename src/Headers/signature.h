/*
 * Specification of type signatures
 * Contact: Francis McCabe <frankmccabe@mac.com>
 */
#ifndef _SIGNATURE_H_
#define _SIGNATURE_H_

#include "config.h"
#include <ooio.h>

#define rawInt 'I'
#define rawFloat 'F'
#define rawString 'S'

/*
 * Tuple signature:
 * (<s>...<s>)
 *
 * Function:
 * F<s><s>
 * e.g. F(i)i
 *
 * Raw types:
 * i integer
 * f float
 * s string
 *
 * Named type:
 * T<name>;
 *
 * Type Var
 *
 * v<name>;
 *
 * Universal:
 * U<tv><s>
 *
 * Existential
 * E<tv><s>
 *
 * Constructor
 * C<s><s>
 * e.g., list of t
 * CTlist;(Vt;)
 */

typedef enum{
  intSig = 'i',				/* Raw integer */
  fltSig = 'f',				/* Raw floating point */
  strSig = 's',				/* Raw string */
  tVSig = 'v',				/* Type variable, followed by a name */
  usrSig = 'T',				/* Simple type name */
  conSig = 'C',				/* Type constructor */
  funSig = 'F',				/* Arrow type */
  allSig = 'U',				/* Universal type */
  exSig = 'E',				/* Existential type */
  tplSig = '(',				/* tuple of types */
  escSig = '$'				/* escape signature */
} typeSig;


extern uniChar integerSig[];
extern uniChar stringSig[];
extern uniChar floatSig[];

extern logical validSignature(uniChar *s);
extern retCode functionArity(uniChar *sig,int32 *arity);
extern retCode tupleArity(uniChar *sig,int32 *arity);
extern retCode skipSig(uniChar *sig,int32 *start,int32 end);
extern retCode showSignature(ioPo out,uniChar *sig,int32 *start,int32 end);

#endif

