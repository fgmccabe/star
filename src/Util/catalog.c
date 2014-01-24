/*
 * The catalog structure implementation
 */

#include "config.h"
#include "utils.h"
#include "catalogP.h"
#include <ooio.h>
#include <assert.h>

static poolPo catalogPool=NULL;

static uniChar *baseResolver(catalogPo cat,uniChar *name);
static retCode addToCatalog(catalogPo cat,uniChar *name,uniChar *url);

void initCatalog()
{
  catalogPool = newPool(sizeof(CatalogRec),2);
}

catalogPo createCatalog(uniChar *url,resolverFn resolver,addFn adder)
{
  catalogPo cat = (catalogPo)allocPool(catalogPool);
  cat->url = uniIntern(url);
  cat->resolver = resolver;
  cat->adder = adder;
  cat->contents = NewHash(15,(hashFun)uniHash,(compFun)uniCmp,NULL);
  return cat;
}

uniChar *catalogUrl(catalogPo cat)
{
  return cat->url;
}

uniChar *catalogResolve(catalogPo cat,uniChar *name)
{
  return cat->resolver(cat,name);
}

retCode catalogAddEntry(catalogPo cat,uniChar *name,uniChar *url)
{
  cat->adder(cat,name,url);
  return Ok;
}

catalogPo newCatalog(uniChar *base)
{
  return createCatalog(uniIntern(base),baseResolver,addToCatalog);
}

retCode addToCatalog(catalogPo cat,uniChar *name,uniChar *url)
{
  return hashPut(cat->contents,uniIntern(name),uniIntern(url));
}

uniChar *baseResolver(catalogPo cat,uniChar *name)
{
  return (uniChar*)hashGet(cat->contents,name);
}

typedef struct {
  ioPo logFile;
} DumpInfoRec;

static retCode dumpCatRef(void *n,void *r,void *c)
{
  DumpInfoRec *info = (DumpInfoRec*)c;
  ioPo out = info->logFile;

  return outMsg(out,"%U -> %U",n,r);
}

void dumpCatalog(catalogPo cat)
{
  outMsg(logFile,"catalog: %U\n",cat->url);
  DumpInfoRec info = {.logFile=logFile};
  ProcessTable(dumpCatRef,cat->contents,&info);
  flushOut();
}
