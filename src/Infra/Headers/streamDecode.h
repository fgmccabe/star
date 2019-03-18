//
// Created by Francis McCabe on 2019-03-17.
//

#ifndef STAR_STREAMDECODE_H
#define STAR_STREAMDECODE_H

#include "encoding.h"

retCode skipEncoded(ioPo in, char *errorMsg, long msgLen);
retCode copyEncoded(ioPo in, ioPo out, char *errorMsg, long msgLen);
retCode decodeNm(ioPo in, char *buffer, integer buffLen);


#endif //STAR_STREAMDECODE_H
