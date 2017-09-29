/*
  Manifest & repository handling
  Copyright (c) 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <stdlib.h>
#include "cafe.h"
#include "jsonEvent.h"
#include "manifestP.h"
#include "pkg.h"

// Use the JSON event parser to parse a manifest file and build up a manifest structure

static retCode startManifest(void *cl);
static retCode endManifest(void *cl);
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
  startManifest,
  endManifest,
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

static poolPo manifestPool = NULL;
static poolPo versionPool = NULL;
static poolPo filePool = NULL;

static hashPo manifest;

char repoDir[MAXFILELEN];

void initManifest() {
  manifestPool = newPool(sizeof(ManifestEntryRecord), 128);
  versionPool = newPool(sizeof(ManifestVersionRecord), 128);
  filePool = newPool(sizeof(ManifestFileRecord), 128);
  manifest = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}


// Manage the manifest

manifestEntryPo newManifestEntry(char *name) {
  manifestEntryPo entry = (manifestEntryPo) allocPool(manifestPool);
  uniCpy((char *) &entry->package, NumberOf(entry->package), name);
  entry->versions = NewHash(1, (hashFun) uniHash, (compFun) uniCmp, NULL);
  return entry;
}

manifestEntryPo manifestEntry(char *package) {
  return (manifestEntryPo) hashGet(manifest, package);
}

manifestEntryPo getEntry(const char *name) {
  manifestEntryPo entry = (manifestEntryPo) hashGet(manifest, (void *) name);

  if (entry == NULL) {
    entry = newManifestEntry((char *) name);
    hashPut(manifest, &entry->package, entry);
  }
  return entry;
}

manifestVersionPo newVersion(const char *version) {
  manifestVersionPo vEntry = (manifestVersionPo) allocPool(versionPool);
  uniCpy((char *) &vEntry->version, NumberOf(vEntry->version), version);
  vEntry->resources = NewHash(3, (hashFun) uniHash, (compFun) uniCmp, NULL);
  return vEntry;
}

static retCode pickAny(void *n, void *r, void *c) {
  manifestVersionPo *tgt = (manifestVersionPo *) c;
  *tgt = (manifestVersionPo) r;
  return Eof;
}

manifestVersionPo manifestVersion(char *package, char *version) {
  manifestEntryPo entry = manifestEntry(package);

  if (entry != NULL) {
    if (uniCmp(version, "*") == same) {
      manifestVersionPo deflt = NULL;
      ProcessTable(pickAny, entry->versions, &deflt);
      return deflt;
    } else
      return (manifestVersionPo) hashGet(entry->versions, version);
  } else
    return NULL;
}

manifestFilePo newManifestResource(const char *kind, const char *fileNm) {
  manifestFilePo f = (manifestFilePo) allocPool(filePool);

  uniCpy((char *) &f->kind, NumberOf(f->kind), kind);
  uniCpy((char *) &f->fn, NumberOf(f->fn), fileNm);

  return f;
}

void addResource(manifestVersionPo version, const char *kind, const char *fileNm) {
  manifestFilePo f = newManifestResource(kind, fileNm);

  hashPut(version->resources, &f->kind, f);
}

char *manifestResource(char *package, char *version, char *kind, char *fl, long flLen) {
  manifestVersionPo v = manifestVersion(package, version);

  if (v != NULL) {
    manifestFilePo f = hashGet(v->resources, kind);

    if (f != NULL) {
      strMsg(fl, flLen, "%s/%s", repoDir, &f->fn);
      return fl;
    } else
      return NULL;
  } else
    return NULL;
}

char *packageCodeFile(char *package, char *version, char *flNm, long flLen) {
  return manifestResource(package, version, "code", flNm, flLen);
}

typedef enum {
  initial,
  inPackage,
  inVersion,
  inDetail,
  inResource
} ParseState;

static char *stNames[] = {"initial", "inPackage", "inVersion", "inDetail", "inResource"};

typedef struct {
  char pkg[MAX_SYMB_LEN]; // Package name
  manifestEntryPo entry;
  char ver[MAX_SYMB_LEN];
  manifestVersionPo version;
  char kind[MAX_SYMB_LEN];
  ParseState state;
} ParsingState, *statePo;

retCode loadManifest(char *dir) {
  uniCpy(repoDir, NumberOf(repoDir), dir);
  initManifest();
  initPackages();

  char manifestName[MAXFILELEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", dir);

  ioPo inFile = openInFile(manifestName, utf8Encoding);

  if (inFile != NULL) {
    ParsingState info;
    yyparse(inFile, &manEvents, &info);
    return Ok;
  } else
    return Error;
}

void loadDefltManifest() {
  if (loadManifest(repoDir) != Ok) {
    outMsg(logFile, "error in loading repository from %s", repoDir);
    exit(99);
  }
}

retCode startManifest(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  //logMsg(logFile, "Starting parse of manifest");
  return Ok;
}

retCode endManifest(void *cl) {
  //logMsg(logFile, "Ending parse of manifest");
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

  //logMsg(logFile, "Starting collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      info->state = inPackage;
      uniCpy((char *) &info->pkg, NumberOf(info->pkg), "");
      break;
    case inPackage:
      uniCpy((char *) &info->ver, NumberOf(info->ver), "");
      info->state = inVersion;
      break;
    case inVersion:
      uniCpy((char *) &info->kind, NumberOf(info->kind), "");
      info->state = inResource;
      break;
    case inResource:
      break;

    default:
      return Error;
  }
  return Ok;
}

retCode endCollection(void *cl) {
  statePo info = (statePo) cl;

  //logMsg(logFile, "Ending collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      return Error;
    case inPackage:
      info->state = initial;
      break;
    case inVersion:
      info->state = inPackage;
      break;
    case inResource:
      info->state = inVersion;
      break;
    case inDetail:
      info->state = inResource;
      break;
    default:
      return Error;
  }

  return Ok;
}

retCode startArray(void *cl) {
  return Ok;
}

retCode endArray(void *cl) {
  return Ok;
}

retCode startEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  //logMsg(logFile, "Starting entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case inPackage: {
      info->entry = getEntry(name);
      break;
    }
    case inVersion:
      uniCpy((char *) &info->ver, NumberOf(info->ver), name);
      info->version = newVersion(name);
      hashPut(info->entry->versions, &info->version->version, info->version);
      break;
    case inResource:
      uniCpy((char *) &info->kind, NumberOf(info->kind), name);
      info->state = inDetail;
      break;
    case inDetail:
      return Error; // expecting a text, not a collection
    default:
      return Error;
  }
  return Ok;
}

retCode endEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  //logMsg(logFile, "Ending entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case inDetail:
      info->state = inResource;
      break;
    case inResource:
    case inVersion:
    case inPackage:
    default:
      break;
  }

  return Ok;
}

retCode numEntry(double dx, void *cl) {
  return Ok;
}

retCode boolEntry(logical trueVal, void *cl) {
  return Ok;
}

retCode txtEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  //logMsg(logFile, "Text entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    default:
      return Error;
    case inDetail:
      addResource(info->version, (char *) &info->kind, name);
      break;
  }
  return Ok;
}

retCode nullEntry(void *cl) {
  return Ok;
}

retCode errorEntry(const char *name, void *cl) {
  logMsg(logFile, "Error: %s", name);
  return Ok;
}

void defltRepoDir() {
  if (uniIsLit(repoDir, "")) { // overridden?
    char *dir = getenv("STAR_DIR"); /* pick up the installation directory */
    if (dir == NULL)
      dir = LODIR;                  /* Default installation path */
    uniCpy(repoDir, NumberOf(repoDir), dir);
  }
}
