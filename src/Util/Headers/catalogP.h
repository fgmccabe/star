#ifndef _CATALOG_P_H_
#define _CATALOG_P_H_

#include "config.h"
#include "catalog.h"
#include <ooio.h>

typedef uniChar *(*resolverFn)(catalogPo cat,uniChar *name);
typedef retCode (*addFn)(catalogPo cat,uniChar *name,uniChar *url);

typedef struct _catalog_ {
  uniChar *url;
  resolverFn resolver;
  addFn adder;
  hashPo contents;
} CatalogRec;

extern void initCatalog();
extern void dumpCatalog(catalogPo cat);
extern catalogPo createCatalog(uniChar *url,resolverFn resolver,addFn adder);

#endif



