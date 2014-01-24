#ifndef _CODE_H_
#define _CODE_H_

#include "config.h"
#include <ooio.h>

typedef uint16 *insPo;

typedef struct _method_ *methodPo;
typedef struct _closureRec_ *closurePo;
typedef struct _constant_ *constantPo;

typedef void (*jitCode)();
typedef integer (*cafeFun)();
typedef cafeFun (*pkgFun)();

//typedef struct _scan_table_ *scanTablePo;
//typedef closurePo (*scavengePo)(closurePo clos); /* A scavenger function */
//typedef void (*lScanFun)(framePo fp);	 /* locals scanner function */

#define CHAR_SIZE (sizeof(uniChar))
#define INTEGER_SIZE (sizeof(int32))
#define LONG_SIZE (sizeof(int64))
#define DOUBLE_SIZE (sizeof(double))

#endif
