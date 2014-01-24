/*
 * Handle compilation of theta environments
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "meta.h"
#include "compile.h"
#include "package.h"
#include "codegen.h"
#include "escapes.h"
#include "heap.h"
#include <iostr.h>

#undef debug
#ifdef TRACEASSM
#define debug(Op) if(debugCodeGen){ Op; }
#else
#define debug(Op)
#endif

/*
 * Compile the definitions in a theta environment. This is where we handle the
 * compilation of lambdas.
 */

static retCode fixupFree(dictPo dict,dictPo outer,mtdPo mtd,int vOffset);

typedef retCode (*comp)(uniChar *name,sxPo def,
			lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
			uniChar *path,dictPo dict,
			dictPo *funDict,
			mtdPo mtd);

static retCode compileClosure(sxPo def,
			      contFun cont,varInfoPo var,
			      uniChar *path,
			      dictPo thetaDict,
			      dictPo *fnDict,
			      mtdPo outer,
			      comp progCompiler);

static retCode compFunction(uniChar *name,sxPo def,
			    lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
			    uniChar *path,dictPo dict,
			    dictPo *funDict,mtdPo mtd);
static retCode compMemo(uniChar *name,sxPo def,
			lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
			uniChar *path,dictPo dict,
			dictPo *funDict,mtdPo mtd);
static retCode compProcedure(uniChar *name,sxPo def,
			     lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
			     uniChar *path,dictPo dict,
			     dictPo *funDict,
			     mtdPo mtd);

static retCode compileImport(sxPo import,uniChar *path,
			     dictPo dict,mtdPo mtd);

retCode compileTheta(lxPo defs,uniChar *path,
		     dictPo dict,dictPo outer,
		     compileFun bound, sxPo *expected,sxPo cl,
		     exitPo exit,
		     mtdPo mtd,
		     contFun cont,void *ccl)
{
  int count = sxLength(defs);
  dictStatePo thetaState = dictState(dict);
  retCode ret = Ok;

  // Phase I: declare variables in theta dictionary
  for(int ix=0;ret==Ok && ix<count;ix++){
    sxPo def = sxEl(defs,ix);

    if(sxIsImport(def))
      ret = compileImport(def,path,dict,mtd);
    else if(sxIsTypeDef(def))
      ret = compileTypeDef(def,path,dict);
    else if(sxIsFunction(def)){
      string name = sxFunName(def);
      locationPo loc = sxLoc(def);
      sxPo fType = sxFunType(def);
      varInfoPo info = reserve(loc,name,fType,readOnly,True,general,dict);
      declareVar(info,dict);
    } else if(sxIsMemo(def)){
      string name = sxMemoName(def);
      locationPo loc = sxLoc(def);
      sxPo fType = sxMemoType(def);
      varInfoPo info = reserve(loc,name,fType,readOnly,True,general,dict);
      declareVar(info,dict);
    } else if(sxIsProcedure(def)){
      string name = sxProcName(def);
      locationPo loc = sxLoc(def);
      sxPo fType = sxProcType(def);
      varInfoPo info = reserve(loc,name,fType,readOnly,True,general,dict);
      declareVar(info,dict);
    } else if(sxIsIsDeclaration(def)){
      sxPo var = sxDeclLval(def);
      locationPo loc = sxLoc(var);
      uniChar *vrName = sxLvalName(def);
      sxPo vrType = sxLvalType(def);
      sourceKind kind = typeRep(vrType);
      declareVar(reserve(loc,vrName,vrType,readOnly,True,kind,dict),
		 dict);
    }
    else
      reportError(sxLoc(def),"not permitted in a let: %A",def);
  }

  dictPo funDicts[count];		/* Allocate space to keep functions */

  // Phase II: generate values
  for(int ix=0;ret==Ok && ix<count;ix++){
    sxPo def = sxEl(defs,ix);

    if(sxIsImport(def))
      ;
    else if(sxIsTypeDef(def))
      ;
    else if(sxIsFunction(def)){
      varInfoPo info = varReference(sxFunName(def),dict);
      assert(info!=Null);

      ret=compileClosure(def,assignVar,info,path,
			 dict,&funDicts[ix],mtd,compFunction);
    }
    else if(sxIsMemo(def)){
      varInfoPo info = varReference(sxMemoName(def),dict);
      assert(info!=Null);

      ret=compileClosure(def,assignVar,info,path,
			 dict,&funDicts[ix],mtd,compMemo);
    }
    else if(sxIsProcedure(def)){
      varInfoPo info = varReference(sxProcName(def),dict);
      assert(info!=Null);

      ret=compileClosure(def,assignVar,info,path,
			 dict,&funDicts[ix],mtd,compProcedure);
    } else if(sxIsIsDeclaration(def)||sxIsVarDeclaration(def)){
      varInfoPo info = varReference(sxDeclaredVarName(def),dict);

      assert(info!=Null);

      ret = compileExp(sxDeclValue(def),&info->type,
		       path,dict,outer,exit,mtd,assignVar,info);
    }
  }

  // Phase III: handle fixups
  for(int ix=0;ret==Ok && ix<count;ix++){
    sxPo def = sxEl(defs,ix);

    if(sxIsFunction(def)){
      varInfoPo info = varReference(sxFunName(def),dict);
      assert(info!=Null);

      ret = fixupFree(funDicts[ix],dict,mtd,info->l.off);
    }
    else if(sxIsMemo(def)){
      varInfoPo info = varReference(sxMemoName(def),dict);
      assert(info!=Null);

      ret = fixupFree(funDicts[ix],dict,mtd,info->l.off);
    }
    else if(sxIsProcedure(def)){
      varInfoPo info = varReference(sxProcName(def),dict);
      assert(info!=Null);

      ret = fixupFree(funDicts[ix],dict,mtd,info->l.off);
    }
  }

  if(ret==Ok)
    ret = bound(cl,expected,path,dict,outer,exit,mtd,cont,ccl);

  resetDict(dict,thetaState);
  return ret;
}

/*
 * A function closure consists of a structure that looks like:
 * 
 * +--------+--------------------+
 * | <code> | Free Variables ... |
 * +--------+--------------------+
 *      \
 *       \
 *        \
 *         V
 *   +-----+-------------------+
 *   | G/C | Function Code     |
 *   +-----+-------------------+
 *      /
 *     v
 * +---------+  
 * | GC code |
 * +---------+
 *
 * The G/C code involves evacuation of the closure, scavenging the closure and
 * marking the locals.
 */

static retCode initClosureVar(uniChar *name,varInfoPo var, void *cl);

typedef struct {
  dictPo dict;
  lxPo free;
  uniChar *path;
  mtdPo mtd;
} FreeCollectRecord;

retCode compileClosure(sxPo def,
		       contFun cont,varInfoPo var,uniChar *path,
		       dictPo thetaDict,
		       dictPo *fnDict,
		       mtdPo outer,
		       comp compiler)
{
  uniChar *name = var->name;
  mtdPo mtd = newMethod(name);
  assemPo code = methodCode(mtd);

  lPo catch = newLbl(code,genSym(".C"));
  lPo scan = newLbl(code,genSym(".S"));
  lPo evac = newLbl(code,genSym(".E"));
  lPo scav = newLbl(code,genSym(".V"));
  uniChar buffer[1024];
  lPo entryPoint = newLbl(code,strMsg(buffer,NumberOf(buffer),".%U",name));
  locationPo loc = sxLoc(def);

  tryRet(compiler(name,def,scan,evac,scav,entryPoint,catch,path,
		  thetaDict,fnDict,mtd));

  int closureSize = freeSize(*fnDict);

  if(closureSize==0){			/* No free variables, fixed closure */
    uniChar* curr = currSegment(code);
    setSegment(code,genSym(".gc"));
    defineLbl(code,evac);		/* evac returns this */
    AEnterCFun(code,0);
    ARetC(code,R1);

    defineLbl(code,scav);		/* Will never be called */
    ARtnC(code);
    setSegment(code,curr);

    AAlignTo(code,POINTER_SIZE);
    lPo closure = newLbl(code,genSym(".L"));
    defineLbl(code,closure);
    AConstP(code,entryPoint);

    genMethodCode(mtd,entryPoint);
    VarInfoRec src = {.loc=loc,.where=label,.kind=general,.l.lit=closure};
    return cont(loc,&src,var,methodCode(outer)); /* store the code in declared var */
  }
  else{
    lPo failAlloc = newLbl(code,genSym(".Al"));
    lPo allocClos = currLbl(code,genUSym(name));

    assemPo outerCode = methodCode(outer);
    AAllocH(outerCode,R1,closureSize,failAlloc);
    VarInfoRec src = {.loc=loc,.where=registr,.kind=general,.l.reg=R1};
    AStLbl(outerCode,R1,0,entryPoint);
    cont(loc,&src,var,outerCode);		/* store closure in locals */

    FreeCollectRecord cl = {.dict=thetaDict,
			    .free=nil,.path=path,
			    .mtd=outer};

    processDict(*fnDict,initClosureVar,&cl);

    uniChar *curr = currSegment(outerCode);
    uniChar *gcSeg = genSym(".gc");
    setSegment(outerCode,gcSeg);

    defineLbl(outerCode,failAlloc);	/* We start filling in the block */
    AGc(outerCode,closureSize);
    gcCallSite(outer,*fnDict);		/* heap alloc needs call site */
    ALd(outerCode,ENV,FP,ENV_ARG_OFFSET); /* reload the environment */
    AJmp(outerCode,allocClos);		/* try again to allocate */

    genClosEvac(mtd,entryPoint,evac,scan,scav,cl.free,*fnDict,closureSize);
    genClosScav(mtd,scav,cl.free,*fnDict);
    genCatchBlocks(mtd,catch);
    setSegment(outerCode,curr);
    genMethodCode(mtd,entryPoint);
  }
    
  return Ok;
}

static retCode initClosureVar(uniChar *name,varInfoPo var, void *cl)
{
  FreeCollectRecord *c = (FreeCollectRecord *)cl;

  if(isVrFree(var)){
    locationPo loc = vrLoc(var);

    VarInfoRec tgt = {.loc=loc,.where=basedVar,.base=R1,.l.off=var->l.off};

    if(!(isRawType(var->type))){		/* initialize to help GC */
      tryRet(compileExp(voidCon,&voidType,
			c->path,c->dict,c->dict,Null,c->mtd,
			assignVar,&tgt));
    }

    c->free = mCons(mId(loc,name),c->free);
  }
  return Ok;
}

retCode declareArgs(lxPo args,dictPo fDict,dictPo outer,mtdPo mtd)
{
 // Pick out the arguments and declare them as local variables
  retCode status = Ok;
  int offset = ARG_OFFSET;
  int argCount = sxLength(args);

  // Declare in correct order, args are pushed in reverse order
  for(int ix=0;status==Ok && ix<argCount;ix++){
    sxPo arg = sxEl(args,ix);
    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg))){
      uniChar *aName = sxIden(sxCastExp(arg));
      sxPo aType = sxCastType(arg);
      sourceKind kind = typeRep(aType);

      declare(sxLoc(arg),aName,aType,readOnly,True,kind,offset,fDict);

      offset+=typeSize(aType);
    }
    else
      reportError(sxLoc(arg),"expecting argument definition, not: %A",arg);
  }
  return status;
}

long countLocals(sxPo def)
{
  if(sxIsFunction(def))
    return countLocalsInExp(sxFunExp(def))+
      countLocalsInPtnArgs(sxFunArgs(def));
  else if(sxIsMemo(def))
    return countLocalsInExp(sxMemoExp(def));
  else if(sxIsProcedure(def))
    return countLocalsInAct(sxProcBody(def))+
      countLocalsInPtnArgs(sxProcArgs(def));
  else{
    assert(False);
    return 0;
  }
}

long countLocalsInDefs(lxPo defs)
{
  long size = 0;
  int count = sxLength(defs);

  for(int ix=0;ix<count;ix++){
    sxPo def = sxEl(defs,ix);

    if(sxIsIsDeclaration(def))
      size+=countLocalsInPtn(sxDeclLval(def));
    else if(sxIsVarDeclaration(def))
      size+=countLocalsInPtn(sxDeclLval(def));
    else if(sxIsFunction(def) || sxIsProcedure(def) || sxIsMemo(def))
      size+=POINTER_SIZE;
  }
  return size;
}

retCode compFunction(uniChar *name,sxPo def,
		     lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
		     uniChar *path,dictPo dict,
		     dictPo *fnDict,
		     mtdPo mtd)
{
  lxPo args = sxFunArgs(def);
  assemPo code = methodCode(mtd);

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,catch);			/* The table of catch blocks */
  AConstP(code,scan);			/* locals scanner */
  AConstP(code,scav);
  AConstP(code,evac);			/* Must be in this order */

  defineLbl(code,entryPoint);		/* This is the function entry point */

  long localsSize = ALIGN(countLocals(def),16);

  AEnterFun(code,localsSize);

  dictPo fDict = *fnDict = funDict(-localsSize,dict);

  retCode status = declareArgs(args,fDict,dict,mtd);

  lPo reentryPoint = currLbl(code,genSym(".R"));
  ExitLabel reentry = {.name=name,.cont=jumpCont,.cl=reentryPoint};
      
  // Compile the program body
  if(status==Ok){
    sxPo resType = Null;
    status = compileExp(sxFunExp(def),&resType,
			path,fDict,dict,&reentry,mtd,returnCont,Null);
  }

  // Generate the locals scanners table
  genLocalScanner(scan,fDict,mtd);

  return status;
}

// A memo function is like a regular zero-arity function. 
// Except that the function 'rewrites' itself with the value so subsequent
// calls simply pick up the value

retCode compMemo(uniChar *name,sxPo def,
		 lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
		 uniChar *path,dictPo dict,
		 dictPo *fnDict,
		 mtdPo mtd)
{
  assemPo code = methodCode(mtd);

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,catch);			/* The table of catch blocks */
  AConstP(code,scan);			/* locals scanner */
  AConstP(code,scav);
  AConstP(code,evac);			/* Must be in this order */

  defineLbl(code,entryPoint);		/* This is the function entry point */

  long localsSize = ALIGN(countLocals(def),16);

  AEnterFun(code,localsSize);

  dictPo fDict = *fnDict = funDict(-localsSize,dict);

  lPo reentryPoint = currLbl(code,genSym(".R"));
  ExitLabel reentry = {.name=name,.cont=jumpCont,.cl=reentryPoint};
      
  // Compile the memo body

  sxPo resType = arrowResType(sxMemoType(def));
  retCode status = compileExp(sxMemoExp(def),&resType,
			      path,fDict,dict,&reentry,
			      mtd,memoCont,resType);

  // Generate the locals scanners table
  genLocalScanner(scan,fDict,mtd);

  return status;
}

retCode compProcedure(uniChar *name,sxPo def,
		      lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
		      uniChar *path,dictPo dict,
		      dictPo *fnDict,
		      mtdPo mtd)
{
  assemPo code = methodCode(mtd);

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,catch);			/* The table of catch blocks */
  AConstP(code,scan);			/* locals scanner */
  AConstP(code,scav);
  AConstP(code,evac);			/* Must be in this order */

  defineLbl(code,entryPoint);		/* This is the function entry point */

  lxPo args = sxFunArgs(def);
  long localsSize = ALIGN(countLocals(def),16);

  AEnterFun(code,localsSize);

  dictPo fDict = *fnDict = funDict(-localsSize,dict);

  retCode status = declareArgs(args,fDict,dict,mtd);

  lPo reentryPoint = currLbl(code,genSym(".R"));
  ExitLabel reentry = {.name=name,.cont=jumpCont,.cl=reentryPoint};
      
  // Compile the program body
  if(status==Ok){
    sxPo resType = Null;
    status = compileAction(sxProcBody(def),&resType,
			   path,fDict,dict,&reentry,mtd,rtnCont,Null);
  }

  // Generate the locals scanners table
  genLocalScanner(scan,fDict,mtd);

  return status;
}

retCode compileImport(sxPo import,uniChar *path,dictPo dict,mtdPo mtd)
{
  uniChar buff[MAXLINE];
  uniChar *url = resolveURI(path,sxImportPkg(import),buff,NumberOf(buff));
  packagePo pkg = findPackage(url);
  if(pkg==Null){
    reportError(sxLoc(import),"cannot import %U",sxImportPkg(import));
    return Error;
  }
  else if(pkgDict(pkg)!=Null) // Only use package if it has a dictionary
    return mergeDict(dict,pkgDict(pkg));
  else
    return Error;
}

typedef struct {
  dictPo outer;
  assemPo code;
  int total;
  int rootOff;
} FixRec;

static retCode fixupEntry(uniChar *name,varInfoPo var,void *cl)
{
  FixRec *fix = (FixRec*)cl;
  assemPo code = fix->code;

  if(isVrFree(var)){
    varInfoPo info = search(name,fix->outer);

    assert(info!=Null);

    if(fix->rootOff!=0){
      ALd(code,TRM,FP,fix->rootOff);
      fix->rootOff=0;
    }

    switch(info->kind){
    case general:
      ALd(code,R0,FP,vrInfOffset(info));
      ASt(code,TRM,vrInfOffset(var),R0);
      fix->total+=POINTER_SIZE;
      break;
    case rawChar:
      ALdC(code,R0,FP,vrInfOffset(info));
      AStC(code,TRM,vrInfOffset(var),R0);
      fix->total+=CHAR_SIZE;
      break;
    case rawInt:
      ALdI(code,R0,FP,vrInfOffset(info));
      AStI(code,TRM,vrInfOffset(var),R0);
      fix->total+=INTEGER_SIZE;
      break;
    case rawLong:
      ALdL(code,R0,FP,vrInfOffset(info));
      AStL(code,TRM,vrInfOffset(var),R0);
      fix->total+=LONG_SIZE;
      break;
    case rawFloat:
      ALdD(code,FPR0,FP,vrInfOffset(info));
      AStD(code,TRM,vrInfOffset(var),FPR0);
      fix->total+=DOUBLE_SIZE;
      break;
    default:
      reportError(vrLoc(info),"cannot build free var %V",info);
      return Error;
    }
  }
  return Ok;
}

  
retCode fixupFree(dictPo dict,dictPo outer,mtdPo mtd,int vOffset)
{
  assemPo code = methodCode(mtd);
  FixRec fix = {.outer=outer, .code=code, .total=0, .rootOff=vOffset};

  return processDict(dict,fixupEntry,&fix);
}


