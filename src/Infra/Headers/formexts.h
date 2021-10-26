//
// Created by Francis McCabe on 1/14/18.
//

#ifndef STAR_FORMEXTS_H
#define STAR_FORMEXTS_H

#include "formio.h"

retCode genQuotedChr(ioPo f, void *data, long depth, long precision, logical alt);
retCode genQuotedStr(ioPo f, void *data, long depth, long precision, logical alt);
logical needQuoting(char *str, integer len);

#endif //STAR_FORMEXTS_H
