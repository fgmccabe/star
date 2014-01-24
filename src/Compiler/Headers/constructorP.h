#ifndef _CONSTRUCTOR_P_H_
#define _CONSTRUCTOR_P_H_

/*
 * Private interface for constructors
 */

typedef struct _constructor_code_ {
  caseFun caseJumper;			/* Executed on a case statement */
} ConstructorBlock;

#endif
