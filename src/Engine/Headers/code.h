#ifndef _CODE_H_
#define _CODE_H_

#include "config.h"
#include "io.h"
#include "term.h"

typedef uint16 insWord, *insPo;

typedef struct _method_ *methodPo;
typedef struct _closureRec_ *closurePo;
typedef struct _constant_ *constantPo;

typedef void (*jitCode)();
typedef integer (*cafeFun)();
typedef cafeFun (*pkgFun)();


#endif
