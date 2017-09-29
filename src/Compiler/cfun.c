/*
 * Define the C function library
 */
#include <ooio.h>
#include <stdlib.h>

#include "funlibs.h"
#include "hash.h"
#include "pool.h"

static hashPo libFuns;
static hashPo libVars;

void initLibFuns()
{
  libFuns = NewHash(255,(hashFun)uniHash, (compFun)uniCmp,NULL);
  libVars = NewHash(255,(hashFun)uniHash, (compFun)uniCmp,NULL);
}

logical isLibVar(char *name)
{
  return Search(name,libVars)!=Null;
}

varInfoPo findLibVar(char *name)
{
  return (varInfoPo)Search(name,libVars);
}

void defineLibVar(char *name,sxPo type,void *address)
{
  char nameU[1024];

  _uni((unsigned char*)name,nameU,NumberOf(nameU));
  char * libName = uniIntern(nameU);

  varInfoPo libRec = (varInfoPo)malloc(sizeof(VarInfoRec));
  libRec->where = label;
  libRec->l.bx = address;
  libRec->kind = fixed;
  libRec->access = readOnly;
  libRec->name = libName;
  libRec->type = type;

  if(libVars==Null)
    initLibFuns();

  Install(libName,libRec,libVars);
}

logical isLibFun(char *name)
{
  return Search(name,libFuns)!=Null;
}

varInfoPo findLibFun(char *name)
{
  return (varInfoPo)Search(name,libFuns);
}

void defineLibFun(char *name,sxPo type,cFunPo fun)
{
  char nameU[1024];

  _uni((unsigned char*)name,nameU,NumberOf(nameU));
  char * libName = uniIntern(nameU);

  varInfoPo libRec = (varInfoPo)malloc(sizeof(VarInfoRec));
  libRec->where = label;
  libRec->l.bx = fun;
  libRec->kind = fixed;
  libRec->access = readOnly;
  libRec->name = libName;
  libRec->type = type;

  if(libFuns==Null)
    initLibFuns();

  Install(libName,libRec,libFuns);
}

