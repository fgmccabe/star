//
// Created by Francis McCabe on 8/25/18.
//

#ifndef STAR_STRNG_H
#define STAR_STRNG_H

#include "object.h"

typedef struct _strg_object_ *strgPo;

extern char * strgVal(strgPo s);
extern integer strgLen(strgPo s);
extern strgPo newStrng(integer length,char *txt);
extern strgPo newStr(char *txt);

extern classPo strgClass;

#ifdef VERIFY_OBJECT
#define O_STRG(c) ((strgPo)(checkCast((c),strgClass)))
#else
#define O_STRG(c) ((strgPo)(c))
#endif

#endif //STAR_STRNG_H
