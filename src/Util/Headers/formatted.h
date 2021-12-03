//
// Created by Francis McCabe on 11/26/21.
//

#ifndef STAR_FORMATTED_H
#define STAR_FORMATTED_H

#include "ooio.h"

retCode
formatDigits(sign sign, const char *digits, integer digitLen, integer precision, const char *format, integer formatLen,
             char *out, integer outLen, integer *pos);

#endif //STAR_FORMATTED_H
