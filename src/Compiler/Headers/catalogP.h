//
// Created by Francis McCabe on 7/1/17.
//

#ifndef CAFE_CATALOGP_H
#define CAFE_CATALOGP_H

#include "catalog.h"
#include "cafeOptions.h"

typedef struct _catalog_ {
  hashPo entries;
  catalogPo deflt;
} Catalog;

typedef struct _catalog_entry_ {
  char pkg[MAX_SYMB_LEN];
  char ver[MAX_SYMB_LEN];
  char srcFl[MAXFILELEN];
} CatalogEntry;

#endif //CAFE_CATALOGP_H
