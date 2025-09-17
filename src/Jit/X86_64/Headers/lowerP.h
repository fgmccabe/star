//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "abort.h"
#include "jitP.h"
#include "lower.h"
#include "macros.h"
#include "x86_64P.h"

/* Register allocation for x64:
 *
 * RAX = return register
 * RDI 1st argument
 * RSI 2nd argument
 * RDX 3rd argument
 * RCX 4th argument
 * R8  Constants vector
 * R9  Args pointer
 * R10 Current stack
 * R11 Current engine
 * RBP Frame pointer
 * RSP = system stack pointer
 */



#endif //STAR_LOWERP_H
