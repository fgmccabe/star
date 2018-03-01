#ifndef _CODE_H_
#define _CODE_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "heap.h"
#include "opcodes.h"

typedef uint16 insWord, *insPo;

typedef struct _method_ *methodPo;
typedef struct _pkg_record_ *pkgPo;

void initCode();

labelPo
defineMtd(insPo ins, integer insCount, char *name, integer arity, normalPo pool, normalPo locals);

extern methodPo C_MTD(termPo t);

typedef void (*jitCode)();
typedef integer (*cafeFun)();
typedef cafeFun (*pkgFun)();

static inline OpCode opCode(insWord w) {
  return (OpCode) w;
}

extern normalPo codeConstants(methodPo mtd);

extern pkgPo loadedPackage(char *package);
extern char *loadedVersion(char *package);

extern pkgPo markLoaded(char *package, char *version);
extern pkgPo createPkg(char *name, char *version);

extern retCode installMethod(pkgPo pkg, labelPo lbl);

#endif
