/*
 * interface to the io system
 */

#include "funlibs.h"

#include <ooio.h>

retCode outUniStr(ioPo io,uniChar *str)
{
  return outText(io,str,uniStrLen(str));
}

void initIoFuns()
{
  defineLibVar("__#logFile",fileCafeType,logFile);

  lxPo chrArgs = mCons(fileCafeType,mCons(charType,nil));
  defineLibFun("__#outChar",sxArrowType(Null,chrArgs,voidType),
	       (cFunPo)outChar);

  lxPo intArgs = mCons(fileCafeType,mCons(intType,nil));
  defineLibFun("__#outInt",sxArrowType(Null,intArgs,voidType),
	       (cFunPo)outInt);

  lxPo lngArgs = mCons(fileCafeType,mCons(longType,nil));
  defineLibFun("__#outLong",sxArrowType(Null,lngArgs,voidType),
	       (cFunPo)outInt);

  lxPo fltArgs = mCons(fileCafeType,mCons(floatType,nil));
  defineLibFun("__#outFloat",sxArrowType(Null,fltArgs,voidType),
	       (cFunPo)outFloat);

  lxPo strArgs = mCons(fileCafeType,mCons(stringType,nil));
  defineLibFun("__#outStr",sxArrowType(Null,strArgs,voidType),
	       (cFunPo)outUniStr);
}
