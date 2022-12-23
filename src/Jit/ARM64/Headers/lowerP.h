//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "jitP.h"
#include "arm64P.h"

FlexOp formOperand(vOperand v);

// Code literal vector
#define CL (X28)
// Stack base pointer
#define SB (X27)

#endif //STAR_LOWERP_H
