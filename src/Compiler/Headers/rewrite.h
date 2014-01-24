#ifndef _REWRITE_H_
#define _REWRITE_H_

#include "sexp.h"

/*
 * rewrite should return Ok if it performed a rewrite, Fail if the rewrite does
 * not apply and Error if something went wrong
 */
typedef retCode (*rewriteFun)(sxPo *sx,void *cl);

extern sxPo rewrite(sxPo sx,rewriteFun rewrite,void *cl);

#endif
