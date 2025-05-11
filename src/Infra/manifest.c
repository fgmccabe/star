/*
  Manifest & repository handling
  Copyright (c) 2017-2019. Francis G. McCabe
*/

#include <stdlib.h>
#include <pkgP.h>
#include <decode.h>
#include "manifestP.h"
#include "debug.h"

// Use the stream decoder to parse a manifest file and build up a manifest structure
typedef enum {
  initial,
  inManifest,
  inPackage,
  inVersion,
  inResource,
  inDetail
} ParseState;

typedef enum {
  unknown,
  inCode,
  inSig,
  inSrc,
  ignoreDetail
} ResourceState;

static char *stNames[] = {"initial", "inManifest", "inPackage", "inVersion", "inResource", "inDetail"};

static char *resNmes[] = {"unknown", "inCode", "inSig", "inSrc", "ignore"};

typedef struct {
  char pkg[MAX_SYMB_LEN]; // Package name
  manifestEntryPo entry;
  manifestVersionPo version;
  char kind[MAX_SYMB_LEN];
  ParseState state;
  ResourceState resState;
  integer pkgCount;
  integer versionCount;
  integer resourceCount;
  integer fieldCount;
} ParsingState, *statePo;

tracingLevel traceManifest = noTracing;

static retCode startManifest(void *cl);
static retCode endManifest(void *cl);
static retCode startList(integer arity, void *cl);
static retCode endList(void *cl);
static retCode startEntry(integer arity, void *cl);
static retCode endEntry(void *cl);
static retCode charEntry(codePoint cp, void *cl);
static retCode txtEntry(char *name, integer length, void *cl);
static retCode lblEntry(char *name, integer arity, void *cl);
static retCode recLblEntry(char *name, integer arity, FieldRec fields[], void *cl);
static retCode intEntry(integer ix, void *cl);
static retCode fltEntry(double dx, void *cl);
static retCode badEntry(void *cl);
static retCode errorEntry(const char *name, void *cl);
static retCode closureEntry(void *cl);
static retCode endClosureEntry(void *cl);
static retCode bignumEntry(uint32 *data, integer count, void *cl);

retCode decodeManifest(ioPo in) {
  ParsingState info = {.state=initial};

  DecodeCallBacks decodeCB = {
    startManifest,          // startDecoding
    endManifest,            // endDecoding
    badEntry,               // decVoid
    intEntry,               // decInt
    fltEntry,               // decFlt
    lblEntry,               // decLbl
    recLblEntry,            // record label
    charEntry,              // Character
    txtEntry,               // decString
    startEntry,             // decCon
    endEntry,               // End of constructor entry
    startList,
    endList,
    closureEntry,           // Should not have any closures in the manifest
    endClosureEntry,
    bignumEntry,            // Should not have any big nums here
  };

  return streamDecode(in, &decodeCB, &info, NULL, 0);
}

static poolPo manifestPool = NULL;
static poolPo versionPool = NULL;
static poolPo rsrcPool = NULL;

static hashPo manifest;

char repoDir[MAXFILELEN];

void initManifest() {
  manifestPool = newPool(sizeof(ManifestEntryRecord), 128);
  versionPool = newPool(sizeof(ManifestVersionRecord), 128);
  rsrcPool = newPool(sizeof(ManifestRsrcRecord), 128);
  manifest = newHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

// Manage the manifest

manifestEntryPo newManifestEntry(char *name) {
  manifestEntryPo entry = (manifestEntryPo) allocPool(manifestPool);
  uniCpy((char *) &entry->package, NumberOf(entry->package), name);
  entry->versions = newHash(1, (hashFun) uniHash, (compFun) uniCmp, NULL);
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
  vEntry->resources = newHash(3, (hashFun) uniHash, (compFun) uniCmp, (destFun) destRsrc);
  hashPut(entry->versions, &vEntry->version, vEntry);

  return vEntry;
}

integer countVersions(manifestEntryPo entry) {
  return hashSize(entry->versions);
}

static retCode pickAny(void *n, void *r, void *c) {
  manifestVersionPo *tgt = (manifestVersionPo *) c;
  *tgt = (manifestVersionPo) r;
  return Eof;
}

manifestVersionPo manifestVersion(manifestEntryPo entry, char *version) {
  if (uniCmp(version, "*") == same) {
    manifestVersionPo deflt = NULL;
    processHashTable(pickAny, entry->versions, &deflt);
    return deflt;
  } else
    return (manifestVersionPo) hashGet(entry->versions, version);
}

manifestRsrcPo newManifestResource(const char *kind, const char *text, integer length) {
  manifestRsrcPo f = (manifestRsrcPo) allocPool(rsrcPool);

  uniCpy((char *) &f->kind, NumberOf(f->kind), kind);
  f->fn = uniDupl((char *) text, length);
  f->fnLen = length;

  return f;
}

retCode addResource(manifestVersionPo version, const char *kind, const char *rscrc, integer length) {
  manifestRsrcPo f = newManifestResource(kind, rscrc, length);

  return hashPut(version->resources, &f->kind, f);
}

retCode manifestResource(packagePo pkg, char *kind, char *buffer, integer buffLen) {
  return manifestCompatibleResource(pkg->packageName, pkg->version, kind, buffer, buffLen);
}

retCode manifestCompatibleResource(char *pkg, char *version, char *kind, char *buffer, integer buffLen) {
  manifestEntryPo entry = manifestEntry(pkg);
  if (entry != Null) {
    manifestVersionPo v = manifestVersion(entry, version);
    if (v != Null && version != Null) {
      manifestRsrcPo f = hashGet(v->resources, kind);

      if (f != NULL)
        return uniNCpy(buffer, buffLen, f->fn, f->fnLen);
    } else
      return Fail;
  }
  return Fail;
}

retCode addToManifest(packagePo package, char *kind, char *resrc, integer length) {
  manifestEntryPo entry = getEntry(package->packageName);

  manifestVersionPo v = manifestVersion(entry, package->version);
  if (v == NULL) {
    v = newVersion(entry, package->version);
  }

  return addResource(v, kind, resrc, length);
}

static retCode pruneVersions(void *n, void *r, void *c) {
  manifestVersionPo v = (manifestVersionPo) r;

  manifestRsrcPo rsrc = (manifestRsrcPo) hashGet(v->resources, c);
  if (rsrc != Null) {
    hashRemove(v->resources, c);
    if (rsrc->fn != Null) {
      free(rsrc->fn);
      rsrc->fn = Null;
    }
    freePool(rsrcPool, rsrc);
  }
  return Ok;
}

static retCode pruneEntries(void *n, void *r, void *c) {
  manifestEntryPo entry = (manifestEntryPo) r;

  return processHashTable(pruneVersions, entry->versions, c);
}

retCode pruneResources(char *kind) {
  return processHashTable(pruneEntries, manifest, (void *) kind);
}

retCode loadManifest() {
  initManifest();

  char manifestName[MAXFILELEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", repoDir);

  ioPo inFile = openInFile(manifestName, utf8Encoding);

  if (inFile != NULL) {
    retCode ret = decodeManifest(inFile);

    if (traceManifest >= generalTracing)
      dispManifest(logFile);
    return ret;
  } else {
    return Fail;
  }
}

retCode startManifest(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "Starting parse of manifest");
  return Ok;
}

retCode endManifest(void *cl) {
  if (traceManifest >= detailedTracing) {
    logMsg(logFile, "Ending parse of manifest");
    dispManifest(logFile);
    if (isAFile(O_OBJECT(logFile)))
      flushFile(O_FILE(logFile));
  }
  return Ok;
}

retCode startList(integer arity, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "Starting sequence, state = %s, %d elements", stNames[info->state], arity);

  switch (info->state) {
    case initial: {
      info->state = inManifest;
      uniCpy((char *) &info->pkg, NumberOf(info->pkg), "");
      info->pkgCount = arity;
      return Ok;
    }
    case inManifest: {
      return Ok;
    }
    case inPackage: {
      info->versionCount = arity;
      return Ok;
    }
    case inVersion:
      info->state = inResource;
      return Ok;
    case inResource:
    default:
      return Error;
  }
}

retCode endList(void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing) {
    logMsg(logFile, "Ending sequence, state = %s", stNames[info->state]);
  }

  switch (info->state) {
    case initial:
      return Error;

    case inManifest: {
      info->state = initial;
      return Ok;
    }
    case inPackage: {
      return Ok;
    }
    case inResource:
      info->state = inVersion;
      return Ok;

    case inVersion:
      info->state = inPackage;
      return Ok;

    default:
      return Error;
  }
}

retCode startEntry(integer arity, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "Starting entry, state = %s, count=%d", stNames[info->state], arity);

  switch (info->state) {
    case initial:
      return Error;
    case inManifest:
      info->state = inPackage;
      info->versionCount = arity;
      return Ok;
    case inPackage:
      info->state = inVersion;
      return Ok;
    case inVersion:
      info->state = inResource;
      info->resState = unknown;
      return Ok;
    case inResource:
      info->state = inDetail;
      info->resState = unknown;
      return Ok;
    default:
      return Error;
  }
}

retCode endEntry(void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "End of entry, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      return Error;
    case inManifest: {
      return Ok;
    }
    case inPackage:
      info->state = inManifest;
      return Ok;
    case inVersion:
      info->state = inPackage;
      return Ok;
    case inDetail:
      info->resState = unknown;
      info->state = inResource;
      return Ok;
    case inResource:
      info->state = inVersion;
      return Ok;
    default:
      return Error;
  }
}

retCode lblEntry(char *name, integer arity, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "Label, state = %s, name=%s/%d", stNames[info->state], name, arity);

  switch (info->state) {
    case initial:
      return Error;
    case inManifest:
    case inPackage:
    case inVersion:
      return Ok;
    case inDetail:
      if (uniIsLit(name, "code")) {
        info->resState = inCode;
        return Ok;
      } else if (uniIsLit(name, "signature")) {
        info->resState = inSig;
        return Ok;
      } else if (uniIsLit(name, "source")) {
        info->resState = inSrc;
        return Ok;
      } else {
        info->resState = ignoreDetail;
        return Ok;
      }
    default:
      return Error;
  }
}

retCode recLblEntry(char *name, integer arity, FieldRec fields[], void *cl) {
  return Error;
}

// No numeric entries in manifest
retCode intEntry(integer ix, void *cl) {
  return Error;
}

retCode fltEntry(double dx, void *cl) {
  return Error;
}

retCode bignumEntry(uint32 *data, integer count, void *cl) {
  return Error;
}

retCode closureEntry(void *cl) {
  return Error;
}

retCode endClosureEntry(void *cl) {
  return Error;
}

retCode badEntry(void *cl) {
  return Error;
}

retCode charEntry(codePoint cp, void *cl) {
  return Error;
}

retCode txtEntry(char *name, integer length, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest >= detailedTracing)
    logMsg(logFile, "Text entry, state = %s(%s), name=%S", stNames[info->state],
           (info->state == inDetail ? resNmes[info->resState] : ""), name, length);

  char text[length + 1];
  uniNCpy(text, length + 1, name, length);

  switch (info->state) {
    case initial:
      return Error;
    case inManifest: {
      return Ok;
    }
    case inPackage: {
      info->entry = getEntry(text);
      return Ok;
    }
    case inVersion: {
      info->version = newVersion(info->entry, text);
      return Ok;
    }
    case inDetail: {
      switch (info->resState) {
        case inSrc:
          return addResource(info->version, "source", name, length);
        case inCode:
          return addResource(info->version, "code", name, length);
        case inSig:
          return addResource(info->version, "signature", name, length);
        default:
          return Ok;
      }
    }
    default:
      return Error;
  }
}

retCode errorEntry(const char *name, void *cl) {
  logMsg(logFile, "Error: %s", name);
  return Ok;
}

void defltRepoDir() {
  if (uniIsLit(repoDir, "")) { // overridden?
    char *dir = getenv("STAR_DIR"); /* pick up the installation directory */
    if (dir == NULL) {
      char DF[MAXFILELEN];
      getcwd(DF, sizeof(DF));

      while (uniStrLen(DF) > 0) {
        strMsg(repoDir, NumberOf(repoDir), "%s/.star-repo", DF);
        if (isDirectory(repoDir) == Ok)
          return;
        else {
          integer slash = uniLastIndexOf(DF, uniStrLen(DF), '/');
          if (slash > 0) {
            DF[slash] = '\0';
          } else {
            uniCpy(repoDir, NumberOf(repoDir), STARDIR);  /* Default installation path */
            return;
          }
        }
      }
    }
  }
}

typedef struct {
  ioPo out;
  int indent;
  logical first;
} IndentPolicy;

static retCode dumpRsrc(char *k, manifestRsrcPo rsrc, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  return outMsg(policy->out, "%s%p\"%s\":\"%S\"", sep, policy->indent, rsrc->kind, rsrc->fn, rsrc->fnLen);
}

static retCode dumpVersion(char *v, manifestVersionPo vers, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;
  IndentPolicy inner = {.indent=policy->indent + 2, .out=policy->out, .first=True};

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  retCode ret = outMsg(policy->out, "%s%p\"%s\":{\n", sep, policy->indent, vers->version);

  if (ret == Ok)
    ret = processHashTable((procFun) dumpRsrc, vers->resources, &inner);

  if (ret == Ok)
    ret = outMsg(policy->out, "}");
  return ret;
}

static retCode dispEntry(char *v, manifestEntryPo entry, void *cl) {
  IndentPolicy *policy = (IndentPolicy *) cl;
  IndentPolicy inner = {.indent=policy->indent + 2, .out=policy->out, .first=True};

  char *sep = policy->first ? "" : ",\n";
  policy->first = False;

  retCode ret = outMsg(policy->out, "%s%p\"%s\":{\n", sep, policy->indent, entry->package);

  if (ret == Ok)
    ret = processHashTable((procFun) dumpVersion, entry->versions, &inner);

  if (ret == Ok)
    ret = outMsg(policy->out, "}");
  return ret;
}

retCode dispManifest(ioPo out) {
  IndentPolicy policy = {.indent=2, .out=out, .first=True};

  retCode ret = outMsg(out, "manifest{\n");

  if (ret == Ok)
    ret = processHashTable((procFun) dispEntry, manifest, &policy);

  if (ret == Ok)
    ret = outMsg(out, "}\n");
  return ret;
}

typedef struct {
  ioPo out;
  int count;
} EncodeInfo;

static retCode encodeRsrc(char *k, manifestRsrcPo rsrc, void *cl) {
  EncodeInfo *info = (EncodeInfo *) cl;

  info->count++;

  tryRet(encodeCons(info->out, 1));
  tryRet(encodeLbl(info->out, rsrc->kind, 1));
  return encodeStr(info->out, rsrc->fn, rsrc->fnLen);
}

retCode encodeVersion(char *v, manifestVersionPo vers, void *cl) {
  EncodeInfo *info = (EncodeInfo *) cl;
  strBufferPo rsrcBuf = newStringBuffer();
  EncodeInfo rsrc = {.out = O_IO(rsrcBuf), .count = 0};

  info->count++;

  tryRet(processHashTable((procFun) encodeRsrc, vers->resources, &rsrc));

  tryRet(encodeCons(info->out, 2));
  tryRet(encodeTplLbl(info->out, 2));

  tryRet(encodeStr(info->out, vers->version, uniStrLen(vers->version)));

  tryRet(encodeLst(info->out, rsrc.count));
  {
    integer len;
    char *text = getTextFromBuffer(rsrcBuf, &len);
    return outText(info->out, text, len);
  }
}

retCode encodeEntry(char *v, manifestEntryPo entry, void *cl) {
  EncodeInfo *info = (EncodeInfo *) cl;
  strBufferPo versBuf = newStringBuffer();
  EncodeInfo vers = {.out = O_IO(versBuf), .count = 0};

  info->count++;

  tryRet(processHashTable((procFun) encodeVersion, entry->versions, &vers));

  tryRet(encodeCons(info->out, 2));
  tryRet(encodeTplLbl(info->out, 2));

  tryRet(encodeStr(info->out, entry->package, uniStrLen(entry->package)));

  tryRet(encodeLst(info->out, vers.count));
  {
    integer len;
    char *text = getTextFromBuffer(versBuf, &len);
    return outText(info->out, text, len);
  }
}

retCode encodeManifest(ioPo out) {
  strBufferPo buf = newStringBuffer();
  EncodeInfo info = {.out = O_IO(buf), .count=0};

  retCode ret = processHashTable((procFun) encodeEntry, manifest, &info);

  if (ret == Ok) {
    ret = encodeLst(out, info.count);
    if (ret == Ok) {
      integer len;
      char *text = getTextFromBuffer(buf, &len);
      ret = outText(out, text, len);
    }
  }

  return ret;
}

retCode flushManifest() {
  char manifestName[MAXFILELEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", repoDir);

  ioPo outFile = openOutFile(manifestName, utf8Encoding);

  if (outFile != NULL) {
    retCode ret = encodeManifest(outFile);

    if (ret == Ok)
      ret = closeIo(outFile);
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

    resolveFileName(wd, path, uniStrLen(path), repoDir, NumberOf(repoDir));
  } else
    strMsg(repoDir, NumberOf(repoDir), "%s", path);
#ifdef TRACEMANIFEST
  if (traceManifest >= detailedTracing)
    logMsg(logFile, "repository manifest set to %s", repoDir);
#endif
  return Ok;
}
