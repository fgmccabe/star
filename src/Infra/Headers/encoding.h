/*
 * An encoding representation for Star byte code
 */

#ifndef _ENCODING_H_
#define _ENCODING_H_

/*
 * A universal data value encoding scheme. This can be used to represent any
 * value.
 */
#include "ooio.h"
#include "pkg.h"
#include "closure.h"

retCode encodeInt(ioPo out, integer ix);
retCode encodeFlt(ioPo out, double dx);
retCode encodeChar(ioPo out, codePoint cp);
retCode encodeStr(ioPo out, char *dx, integer len);
retCode encodeTxt(ioPo out, char *sx, integer len);
retCode encodeEnum(ioPo out, char *nm);
retCode encodeLbl(ioPo out, char *sx, integer ar);
retCode encodeTplLbl(ioPo out, integer ar);
retCode encodeClosure(ioPo out,closurePo cl);
retCode encodeLst(ioPo out, integer ar);
retCode encodeCons(ioPo out, integer arity);
retCode encodePkgName(ioPo out, packagePo pkg);
#endif
