/*
  Manifest & repository handling
  Copyright (c) 2017-2018. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <stdlib.h>
#include <pkgP.h>
#include "jsonEvent.h"
#include "manifestP.h"
#include "engineOptions.h"


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
static poolPo rsrcPool = NULL;

static hashPo manifest;

char repoDir[MAXFILELEN];

void initManifest() {
  manifestPool = newPool(sizeof(ManifestEntryRecord), 128);
  versionPool = newPool(sizeof(ManifestVersionRecord), 128);
  rsrcPool = newPool(sizeof(ManifestRsrcRecord), 128);
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

static void destRsrc(char *knd, manifestRsrcPo rsrc) {
  freePool(rsrcPool, rsrc);
}

manifestVersionPo newVersion(manifestEntryPo entry, const char *version) {
  manifestVersionPo vEntry = (manifestVersionPo) allocPool(versionPool);
  uniCpy((char *) &vEntry->version, NumberOf(vEntry->version), version);
  vEntry->resources = NewHash(3, (hashFun) uniHash, (compFun) uniCmp, (destFun) destRsrc);
  hashPut(entry->versions, &vEntry->version, vEntry);

  return vEntry;
}

static retCode pickAny(void *n, void *r, void *c) {
  manifestVersionPo *tgt = (manifestVersionPo *) c;
  *tgt = (manifestVersionPo) r;
  return Eof;
}

manifestVersionPo manifestVersion(manifestEntryPo entry, char *version) {
  if (uniCmp(version, "*") == same) {
    manifestVersionPo deflt = NULL;
    ProcessTable(pickAny, entry->versions, &deflt);
    return deflt;
  } else
    return (manifestVersionPo) hashGet(entry->versions, version);
}

manifestRsrcPo newManifestResource(const char *kind, const char *fileNm) {
  manifestRsrcPo f = (manifestRsrcPo) allocPool(rsrcPool);

  uniCpy((char *) &f->kind, NumberOf(f->kind), kind);
  uniCpy((char *) &f->fn, NumberOf(f->fn), fileNm);

  return f;
}

retCode addResource(manifestVersionPo version, const char *kind, const char *rscrc) {
  manifestRsrcPo f = newManifestResource(kind, rscrc);

  return hashPut(version->resources, &f->kind, f);
}

char *manifestResource(packagePo pkg, char *kind) {
  manifestEntryPo entry = manifestEntry(pkg->packageName);

  if (entry != NULL) {
    manifestVersionPo v = manifestVersion(entry, pkg->version);

    if (v != NULL) {
      manifestRsrcPo f = hashGet(v->resources, kind);

      if (f != NULL)
        return f->fn;
    }
  }
  return NULL;
}

char *manifestRsrcFlNm(packagePo pkg, char *kind, char *buffer, integer buffLen) {
  char *rsrc = manifestResource(pkg, kind);

  if (rsrc != Null) {
    return strMsg(buffer, buffLen, "%s/%s", repoDir, rsrc);
  } else
    return Null;
}

retCode addToManifest(packagePo package, char *kind, char *resrc) {
  manifestEntryPo entry = getEntry(package->packageName);

  manifestVersionPo v = manifestVersion(entry, package->version);
  if (v == NULL) {
    v = newVersion(entry, package->version);
  }

  return addResource(v, kind, resrc);
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

retCode loadManifest() {
  initManifest();

  char manifestName[MAXFILELEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", repoDir);

  ioPo inFile = openInFile(manifestName, utf8Encoding);

  if (inFile != NULL) {
    ParsingState info = {.state=initial};
    yyparse(inFile, &manEvents, &info);
    return Ok;
  } else {
    return Fail;
  }
}

retCode startManifest(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  if (traceManifest)
    logMsg(logFile, "Starting parse of manifest");
  return Ok;
}

retCode endManifest(void *cl) {
  if (traceManifest)
    logMsg(logFile, "Ending parse of manifest");
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest)
    logMsg(logFile, "Starting collection, state = %s", stNames[info->state]);

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

  if (traceManifest)
    logMsg(logFile, "Ending collection, state = %s", stNames[info->state]);

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

  if (traceManifest)
    logMsg(logFile, "Starting entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case inPackage: {
      info->entry = getEntry(name);
      break;
    }
    case inVersion:
      uniCpy((char *) &info->ver, NumberOf(info->ver), name);
      info->version = newVersion(info->entry, name);
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

  if (traceManifest)
    logMsg(logFile, "Ending entry, state = %s, name=%s", stNames[info->state], name);

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

  if (traceManifest)
    logMsg(logFile, "Text entry, state = %s, name=%s", stNames[info->state], name);

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
      dir = STARDIR;                  /* Default installation path */
    uniCpy(repoDir, NumberOf(repoDir), dir);
  }
}

typedef struct {
  ioPo out;
  int indent;
  logical first;
} IndentPolicy;

retCode dumpRsrc(char *k, manifestRsrcPo rsrc, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  return outMsg(policy->out, "%s%p\"%Q\":\"%Q\"", sep, policy->indent, rsrc->kind, rsrc->fn);
}

retCode dumpVersion(char *v, manifestVersionPo vers, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;
  IndentPolicy inner = {.indent=policy->indent + 2, .out=policy->out, .first=True};

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  retCode ret = outMsg(policy->out, "%s%p\"%Q\":{\n", sep, policy->indent, vers->version);

  if (ret == Ok)
    ret = ProcessTable((procFun) dumpRsrc, vers->resources, &inner);

  if (ret == Ok)
    ret = outMsg(policy->out, "}");
  return ret;
}

retCode dumpEntry(char *v, manifestEntryPo entry, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;
  IndentPolicy inner = {.indent=policy->indent + 2, .out=policy->out, .first=True};

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  retCode ret = outMsg(policy->out, "%s%p\"%Q\":{\n", sep, policy->indent, entry->package);

  if (ret == Ok)
    ret = ProcessTable((procFun) dumpVersion, entry->versions, &inner);

  if (ret == Ok)
    ret = outMsg(policy->out, "}");
  return ret;
}

retCode dumpManifest(ioPo out) {
  IndentPolicy policy = {.indent=2, .out=out, .first=True};

  retCode ret = outMsg(out, "{\n");

  if (ret == Ok)
    ret = ProcessTable((procFun) dumpEntry, manifest, &policy);

  if (ret == Ok)
    ret = outMsg(out, "}");
  return ret;
}

retCode flushManifest() {
  char manifestName[MAXFILELEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", repoDir);

  ioPo outFile = openOutFile(manifestName, utf8Encoding);

  if (outFile != NULL) {
    retCode ret = dumpManifest(outFile);

    if (ret == Ok)
      ret = closeFile(outFile);
    return ret;
  } else
    return Fail;
}

char *manifestOutPath(packagePo pkg, char *suff, char *buffer, int bufLen) {
  integer hash = pkgHash(pkg);
  return strMsg(buffer, bufLen, "%s%d.%s", pkg->packageName, hash, suff);
}

char *repoRsrcPath(char *name, char *buffer, int bufLen) {
  return strMsg(buffer, bufLen, "%s/%s", repoDir, name);
}

retCode setManifestPath(char *path) {
  if (path[0] != '/') {
    char wd[MAXFILELEN];
    getcwd(wd, sizeof(wd));
    strMsg(repoDir, NumberOf(repoDir), "%s/%s", wd, path);
  } else
    strMsg(repoDir, NumberOf(repoDir), "%s", path);
  return Ok;
}
