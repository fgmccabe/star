//
// Created by Francis McCabe on 2/1/17.
// Implements base64 encoding/decoding
//

#ifndef STAR_BASE64_H
#define STAR_BASE64_H

#include "io.h"

// Decode a stream of characters as base64
retCode decode64(ioPo dst, ioPo src);

// Encode a stream of bytes into base64 characters
retCode encode64(ioPo src,ioPo dst);

#endif //STAR_BASE64_H
