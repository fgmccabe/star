/*
 * Specification of type signatures
 * Contact: Francis McCabe <frankmccabe@mac.com>
 */
#ifndef _SIGNATURE_H_
#define _SIGNATURE_H_

#include "config.h"
#include <ooio.h>

/*
 * Tuple signature:
 * (<s>...<s>)
 *
 * Function:
 * F<s><s>
 * e.g. F(Tinteger;)Tinteger;
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
 * Type Constructor
 * C<s><s>
 * e.g., list of t
 * CTlist;(Vt;)
 */

typedef enum{
  tVrSig = 'v',				/* Type variable, followed by a name */
  usrSig = 'T',				/* Simple type name */
  conSig = 'C',				/* Type constructor */
  funSig = 'F',       /* Arrow type */
  ptnSig = 'P',       /* Pattern arrow type */
  allSig = 'U',				/* Universal type */
  xstSig = 'E',		   	/* Existential type */
  tplSig = '(',				/* tuple of types */
  fceSig = '{',       /* record signature */
  rawSig = 'R',       /* Raw data (followed by size in bytes) */
  repSig = 'A',       /* repeated signature. Data has a count field */
} typeSig;

#define INTEGER_SIG usrSig,'i','n','t','e','g','e','r',';'
#define STRING_SIG usrSig,'s','t','r','i','n','g',';'
#define FLOAT_SIG usrSig,'f','l','o','a','t',';'

extern uniChar integerSig[];
extern uniChar stringSig[];
extern uniChar floatSig[];

extern logical validSignature(uniChar *s);
extern retCode functionArity(uniChar *sig,int32 *arity);
extern retCode tupleArity(uniChar *sig,int32 *arity);
extern retCode skipSig(uniChar *sig,int32 *start,int32 end);
extern retCode showSignature(ioPo out,uniChar *sig,int32 *start,int32 end);
extern retCode showSig(ioPo out,uniChar *sig);
#endif

