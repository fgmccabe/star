//
// Created by Francis McCabe on 7/1/17.
//

#ifndef CAFE_CATALOG_H
#define CAFE_CATALOG_H

#include "ooio.h"


typedef struct _catalog_ *catalogPo;

typedef struct _catalog_entry_ *catEntryPo;

void initCatalog(char *fl);
catalogPo openCatalog(char *fl);

catEntryPo findInCatalog(catalogPo cat, char *pkg, char *version);

#endif //CAFE_CATALOG_H
