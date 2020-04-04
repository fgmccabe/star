//
// Created by Francis McCabe on 2019-03-17.
//

#ifndef STAR_STREAMDECODE_H
#define STAR_STREAMDECODE_H

#include "encoding.h"

retCode skipEncoded(ioPo in, char *errorMsg, long msgLen);
retCode decodeLbl(ioPo in, char *nm, long nmLen, integer *arity, char *errorMsg, integer msgLen);

#ifdef TRACEDECODE
extern tracingLevel traceDecode;
#endif

#endif //STAR_STREAMDECODE_H
