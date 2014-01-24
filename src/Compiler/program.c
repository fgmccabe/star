/*
 * compile a program and execute its main program
 */

#include <sys/mman.h>

#include "compiler.h"
#include "file.h"
#include <ooio.h>

#include "compile.h"
#include "packageP.h"
#include "codegen.h"
#include "heapP.h"
#include "dict.h"
#include "type.h"

#include <stdlib.h>

static sxPo refactorContent(lxPo defs,int argc,char **argv);


retCode compileAndGo(uniChar *path,int argc, char **argv)
{
  lxPo defs = parseContent(path);

  if(defs!=Null){
    if(parseOnly)
      return outMsg(logFile,"Parsed %U is %#S\n",path,defs);

    if(defs!=Null && sxLength(defs)>0){
      sxPo mainProg = refactorContent(mCons(voidSpec,defs),argc,argv);

      if(mainProg!=Null){
	mtdPo mtd = newMethod(MainName);
	assemPo code = methodCode(mtd);
	lPo entryPoint = newLbl(code,uniNewStr((unsigned char*)"$START"));
	defineLbl(code,entryPoint);
	
	long localSize = ALIGN(countLocalsInDefs(defs),16);
      
	AEnterCFun(code,localSize);	/* we are implementing a C function */
      
	dictPo dict = funDict(-localSize,rootDict);
      
	int retVal = 0;
	sxPo resType = Null;

	compileAction(mainProg,&resType,path,dict,rootDict,
		      Null,mtd,returnMain,&retVal);

	if(compileOnly){
	  outMsg(logFile,"Program: %U\n",mainProg);
	  dumpPkgCode(code);
	  return Ok;
	}
	else if(isErrorFree()){
	  cafeFun fun = generateCode(code,entryPoint);
	  double time = getNanoTime();
	  integer reslt = fun();
	  time = getNanoTime()-time;
	  
	  if(reslt!=0)
	    outMsg(logFile,"%d exit code",reslt);
	  outMsg(logFile,"%f secs\n",time);
	  return Ok;
	}
	else
	  return Error;
      }
      else
	return Error;
    }
    else
      return Error;
  }
  else{
    outMsg(logFile,"cannot find %U\n",path);
    return Error;
  }
}

static sxPo findMainProc(lxPo defs)
{
  if(defs==nil)
    return Null;
  else{
    sxPo def = sxHead(defs);
    if(sxIsProcedure(def)){
      if(uniCmp(sxProcName(def),MainName)==0)
	return def;
    }
    return findMainProc(sxTail(defs));
  }
}


/*
 * We transform the program into a form that handles the actual call args
 * which casts the appropriate elements into a call to main:
 *
 * function fib(X:#int):#int => ....
 *
 * procedure main(X:#int){ ...
 *
 * becomes
 *
 * let {
 *   function fib(X:#int) ...
 *   procedure main(X:#int) ...
 * } in main(string2int(argv[0]))
 *
 * This action is then compiled and entered by the go phase of compile'n go
 *
 */

sxPo refactorContent(lxPo defs,int argc,char **args)
{
  sxPo mainDef = findMainProc(defs);

  if(mainDef==Null){
    reportError(Null,"cannot find a definition of main");
    return Null;
  }
  else{
    sxPo mainType = sxProcType(mainDef);
    locationPo mainLoc = sxLoc(mainDef);
    lxPo typeArgs = arrowArgTypes(mainType);

    int count = sxLength(typeArgs);

    if(argc<sxLength(typeArgs)){
      reportError(mainLoc,"insufficient number of arguments\nusage cafe %#S",
		  typeArgs);
      return Null;
    }

    lxPo callArgs = nil;
    for(int ix=count-1;ix>=0;ix--){
      uniChar buffer[1024];
      _uni((unsigned char*)args[ix],buffer,NumberOf(buffer));

      sxPo argType = sxEl(typeArgs,ix);

      if(isRawCharType(argType))
	callArgs = mCons(mChar(mainLoc,args[ix][0]),callArgs);
      else if(isRawIntType(argType)){
	integer i = parseInteger(buffer,uniStrLen(buffer));
	callArgs = mCons(mInt(mainLoc,i),callArgs);
      } else if(isRawLongType(argType)){
	integer i = parseInteger(buffer,uniStrLen(buffer));
	callArgs = mCons(mLong(mainLoc,i),callArgs);
      } else if(isRawFloatType(argType)){
	double d = parseNumber(buffer,uniStrLen(buffer));
	callArgs = mCons(mFloat(mainLoc,d),callArgs);
      }
      else if(isRawStringType(argType))
	callArgs = mCons(mStr(mainLoc,uniDuplicate(buffer)),callArgs);
      else{
	reportError(mainLoc,
		    "types should be integer, float or string, not %T",
		    argType);
	return Null;
      }
    }

    return sxLet(mainLoc,defs,sxCall(mainLoc,MainName,callArgs));
  }
}




