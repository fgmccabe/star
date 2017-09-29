/*
 * manage packages
 */

#include <sys/mman.h>

#include "compiler.h"
#include <ooio.h>

#include "packageP.h"
#include "compile.h"
#include "escapes.h"
#include "assem.h"
#include "type.h"

#include <stdlib.h>

static poolPo packagePool=NULL;
static hashPo build = NULL;

void initPackages()
{
  packagePool = newPool(sizeof(PackageRec),128);
  build = NewHash(63,(hashFun)uniHash,(compFun)uniCmp,NULL);
}

// Function that will populate a dictionary

static packagePo makePackage(char *url)
{
  packagePo pkg = (packagePo)allocPool(packagePool);
  pkg->url = url;
  pkg->entry = Null;
  pkg->dict = Null;
  return pkg;
}

extern int ssparse(ioPo file,pkgPo context);

static retCode compilePkgExp(sxPo exp,sxPo *expected,
			     char *path,
			     dictPo dict,dictPo outer,exitPo exit,
			     mtdCxtPo mtd, contFun cont,void *cl);

packagePo findPackage(char *path)
{
  packagePo pkg = hashGet(build,path);

  if(pkg==Null){
    sxPo content = parseContent(path);

    if(content!=Null && sxLength(content)>0){
      path = uniIntern(path);
      packagePo pkg = makePackage(path);
      hashPut(build,path,pkg);

      mtdCxtPo mtd = newMethod(genUSym(path));
      assemPo code = methodCode(mtd);

      char *curr = currSegment(code);
      setSegment(code,genUSym(path));

      lPo catch = newLbl(code,genSym(".C"));
      lPo scan = newLbl(code,genSym(".S"));
      lPo evac = newLbl(code,genSym(".E"));
      lPo scav = newLbl(code,genSym(".V"));

      AAlignTo(code,POINTER_SIZE);
      AConstP(code,catch);
      AConstP(code,scan);
      AConstP(code,scav);
      AConstP(code,evac);

      lPo entryPoint = newLbl(code,uniNewStr((unsigned char*)"$PACKAGE"));
      defineLbl(code,entryPoint);
    
      long localSize = ALIGN(countLocalsInDefs(content),16);

      AEnterFun(code,localSize);	/* we are implementing a function */

      dictPo dict = funDict(-localSize,rootDict);

      VarInfoRec dictTgt = {.loc=Null,.name=PkgDictVarName,.type=dictType,
			    .access=readOnly,
			    .where=basedVar,.base=FP,.kind=general,
			    .l.off=FIRST_ARG_OFFSET};
      declareVar(&dictTgt,dict);

      // Compile the package contents as a theta
      sxPo pkgType = Null;
      compileTheta(content,path,dict,rootDict,compilePkgExp,
		   &pkgType,sxBlock(Null,content),
		   Null,mtd,rtnCont,Null);
    
      setSegment(code,curr);

      genLocalScanner(scan,dict,mtd);

      packageFun pkger = (packageFun)generateCode(code,entryPoint);
      dictPo pkgDict = newDict();
      pkger(pkgDict);			/* populate the dictionary */
      pkg->dict = pkgDict;
      pkg->entry = pkger;
      popDict(pkgDict);

      return pkg;
    }
  
    outMsg(logFile,"%U is empty\n",path);
    return NULL;
  }
  else
    return pkg;
}

retCode compilePkgExp(sxPo exp,sxPo *expected,char *path,
		      dictPo dict,dictPo outer,exitPo exit,
		      mtdCxtPo mtd, contFun cont,void *cl)
{
  assert(dict!=Null);

  lxPo content = sxBlockContent(exp);
  int numDefs = sxLength(content);
  assemPo code = methodCode(mtd);

  for(int ix=0;ix<numDefs;ix++){
    sxPo def = sxEl(content,ix);

    if(sxIsFunction(def)){
      char * name = sxFunName(def);
      varInfoPo info = varReference(name,dict);

      // Set up the call to declareLit
      genDeclare(code,info,dict);
      break;
    }
    else if(sxIsProcedure(def)){
      char * name = sxProcName(def);
      varInfoPo info = varReference(name,dict);

      // Set up the call to declareLit
      genDeclare(code,info,dict);
      break;
    }
    else if(sxIsVarDeclaration(def)||sxIsIsDeclaration(def)){
      char * name = sxLvalName(sxDeclLval(def));
      varInfoPo info = varReference(name,dict);
      
      // Set up the call to declareLit
      genDeclare(code,info,dict);
      break;
    }
  }
  return Ok;
}

static packagePo loadAssem(char *path)
{
  ioPo file = openURI(path, unknownEncoding);

  pkgPo pkg = newAssem(path);
  int errors = ssparse(file,pkg);

  closeFile(file);			/* close the source file */
  if(!errors)
    return Null;			/* temporary */
  else
    outMsg(logFile,"could not assemble %U\n",path);
  return Null;
}

retCode ppPackage(ioPo io,packagePo pkg,int d)
{
  if(pkg!=NULL)
    return outMsg(io,"%U\n",pkg->url);
  else
    return outMsg(io,"no package given\n");
}

char *pkgPath(packagePo pkg)
{
  return pkg->url;
}

dictPo pkgDict(packagePo pkg)
{
  return pkg->dict;
}

packageFun pkgEntry(packagePo pkg)
{
  return pkg->entry;
}

