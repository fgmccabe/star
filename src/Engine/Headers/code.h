#ifndef _CODE_H_
#define _CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "heap.h"
#include "opcodes.h"

typedef uint16 insWord, *insPo;

typedef struct _method_ *methodPo;

void initCode();

extern methodPo C_MTD(termPo t);

typedef void (*jitCode)();
typedef integer (*cafeFun)();
typedef cafeFun (*pkgFun)();

extern normalPo codeLits(methodPo mtd);

extern termPo getMtdLit(methodPo mtd,integer litNo);

extern integer lclCount(methodPo mtd);

typedef struct _pkg_record_ *pkgPo;

extern pkgPo loadedPackage(char *package);
extern char *loadedVersion(char *package);

extern pkgPo markLoaded(char *package, char *version);
extern pkgPo createPkg(char *name, char *version);

extern methodPo
defineMtd(heapPo H, insPo ins, integer insCount, integer lclCount, labelPo lbl, normalPo pool, normalPo locals);

#endif
