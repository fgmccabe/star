/*
 * top-level of the compiler
 */

#include "compiler.h"
#include "catalogP.h"
#include "metaP.h"
#include "packageP.h"
#include "dict.h"

#include "compile.h"
#include "codegen.h"
#include "escapes.h"
#include "arith.h"
#include "type.h"

static ioPo openStdURI(uniChar *uri,ioEncoding encoding);

static void initStdUri();

void initCompiler()
{
  initPackages();
  initDict();
  initArith();
  initMeta();
  initAssem();
  initMethod();
  initTypes();
  initEscapes();

  initStdUri();

  // Construct a type definition for void ...
  // (type void (void))
  voidCon = mId(Null,VoidName);
  lxPo constructors = mCons(voidCon,nil);
  voidSpec = sxTypeDef(Null,voidCon,constructors);
}

exitPo exitLabel(exitPo exit,uniChar *name)
{
  while(exit!=Null){
    if(uniCmp(name,exit->name)==0)
      return exit;
    else
      exit = exit->outer;
  }
  return Null;
}

static uniChar *CAFE_HOME = NULL;

void setCafeHome(uniChar *home)
{
  CAFE_HOME = home;
}

static ioPo openStdURI(uniChar *uri,ioEncoding encoding)
{
  uniChar scheme[MAXFILELEN];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(CAFE_HOME==NULL){
    outMsg(logFile,"CAFE_HOME not set");
    exit(99);
  }
  else if(parseURI(uri,scheme,NumberOf(scheme),
		   user,NumberOf(user),pass,NumberOf(pass),
		   host,NumberOf(host),&port,path,NumberOf(path),
		   query,NumberOf(query),frag,NumberOf(frag))==Ok &&
	  uniIsLit(scheme,"cafe")){
    uniChar newUri[MAXFILELEN];
    resolveURI(CAFE_HOME,path,newUri,NumberOf(newUri));
    return openURI(newUri,encoding);
  }
  else
    return NULL;
}

static void initStdUri()
{
  if(CAFE_HOME==NULL){
    char *home = getenv("CAFE_HOME");

    if(home!=NULL)
      CAFE_HOME = mkInterned(home);
    else{
      uniChar buff[MAXFILELEN];
      strMsg(buff,NumberOf(buff),"file:%s/share/stdlib/",CAFEDIR);
      CAFE_HOME = uniIntern(buff);
    }
  }
  registerTransducer(mkInterned("cafe"),openStdURI);
}

