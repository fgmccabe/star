#ifndef _CATALOG_H_
#define _CATALOG_H_

#include <ooio.h>
#include <unicode.h>

typedef struct _catalog_ *catalogPo;

extern uniChar *catalogResolve(catalogPo cat,uniChar *url);
extern retCode catalogAddEntry(catalogPo cat,uniChar *name,uniChar *url);
extern uniChar *catalogUrl(catalogPo cat);

extern catalogPo newCatalog(uniChar *url);

#endif

