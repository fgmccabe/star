//
// Created by Francis McCabe on 2019-03-17.
//

#ifndef STAR_STREAMDECODE_H
#define STAR_STREAMDECODE_H

#include "encoding.h"
#include "starOptions.h"

retCode skipEncoded(ioPo in, char *errorMsg, long msgLen);
retCode decodeLbl(ioPo in, char *nm, long nmLen, int32 *arity, char *errorMsg, integer msgLen);
retCode decI32(ioPo in, int32 *rest);

#endif //STAR_STREAMDECODE_H
