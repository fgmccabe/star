/*
 * An encoding representation for Cafe byte code
 */

#ifndef _ENCODING_H_
#define _ENCODING_H_

/*
 * A universal data value encoding scheme. This can be used to represent any
 * value.
 */
#include "engine.h"
#include "signature.h"

retCode encodeInt(ioPo out, integer ix);
retCode encodeFlt(ioPo out, double dx);
retCode encodeStr(ioPo out, char *dx);
retCode encodeTxt(ioPo out, char *sx);
retCode encodePrg(ioPo out, char *sx, integer ar);
retCode encodeStrct(ioPo out, char *sx, integer ar);

codePoint findDelim(char *str,char *choices);

#endif
