#ifndef _ESCAPES_H_
#define _ESCAPES_H_

#include "dict.h"
#include "compile.h"

extern void initEscapes();

typedef retCode (*escapeFun)(locationPo loc,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit,
			     mtdCxtPo mtd,lxPo args,
			     contFun cont,void *cl);

extern logical isEscape(uniChar *name);
extern logical isLibVar(uniChar *name);
extern logical isLibFun(uniChar *name);

extern retCode compileEscape(locationPo loc,uniChar *name,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit,
			     mtdCxtPo mtd,lxPo args,
			     contFun cont,void *cl);

extern retCode compileLibVar(locationPo loc,uniChar *name,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit,
			     mtdCxtPo mtd,
			     contFun cont,void *cl);


extern void defineEscape(char *name, escapeFun escape);

extern void initLibFuns();

extern void genDeclare(assemPo code,varInfoPo info,dictPo dict);

extern retCode compileCCall(sxPo call,sxPo *expected,
			    uniChar *path,
			    dictPo dict,dictPo outer,
			    exitPo exit,
			    mtdCxtPo mtd,
			    contFun cont,void *cl);

#endif
