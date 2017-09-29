//
// Created by Francis McCabe on 7/1/17.
//

#include "catalogP.h"
#include "jsonEvent.h"

static poolPo catalogPool;
static poolPo catEntryPool;

static catalogPo defltCatalog;

static hashPo catalogs;

void initCatalog(char *catFl){
  catalogPool = newPool(sizeof(Catalog), 16);
  catEntryPool = newPool(sizeof(CatalogEntry), 128);
  catalogs = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}


// Use the JSON event parser to parse a catalog file and build up a catalog

static retCode startCatalog(void *cl);
static retCode endCatalog(void *cl);
static retCode startCollection(void *cl);
static retCode endCollection(void *cl);
static retCode startArray(void *cl);
static retCode endArray(void *cl);
static retCode startEntry(const char *name, void *cl);
static retCode endEntry(const char *name, void *cl);
static retCode numEntry(double dx, void *cl);
static retCode boolEntry(logical trueVal, void *cl);
static retCode txtEntry(const char *name, void *cl);
static retCode nullEntry(void *cl);
static retCode errorEntry(const char *name, void *cl);

JsonCallBacks manEvents = {
  startCatalog,
  endCatalog,
  startCollection,
  endCollection,
  startArray,
  endArray,
  startEntry,
  endEntry,
  txtEntry,
  numEntry,
  boolEntry,
  nullEntry,
  errorEntry
};

typedef struct {
  catalogPo currentCat;
} CatParserData;

static retCode startCatalog(void *cl){
  CatParserData *cd = (CatParserData*)cl;
  cd->currentCat = (catalogPo)allocPool(catalogPool);
  if(defltCatalog==NULL)
    defltCatalog = cd->currentCat;
  return Ok;
}

static retCode endCatalog(void *cl);
static retCode startCollection(void *cl);
static retCode endCollection(void *cl);
static retCode startArray(void *cl);
static retCode endArray(void *cl);
static retCode startEntry(const char *name, void *cl);
static retCode endEntry(const char *name, void *cl);
static retCode numEntry(double dx, void *cl);
static retCode boolEntry(logical trueVal, void *cl);
static retCode txtEntry(const char *name, void *cl);
static retCode nullEntry(void *cl);
static retCode errorEntry(const char *name, void *cl);
