//
// Created by Francis McCabe on 10/21/18.
//
#include <assert.h>
#include <star.h>
#include <engineOptions.h>
#include "srcMapP.h"
#include "jsonEvent.h"
#include "manifestP.h"

static void initSrcMap();

static integer mapEntryHash(srcMapPo mp) {
  return pkgHash(srcMapPkg(mp)) * 37 + srcMapLine(mp);
}

packagePo srcMapPkg(srcMapPo mp) {
  assert(mp != Null);

  return mp->pkg;
}

labelPo srcMapFun(srcMapPo mp) {
  assert(mp != Null);

  return mp->fun;
}

integer srcMapLine(srcMapPo mp) {
  assert(mp != Null);

  return mp->line;
}

integer srcMapPc(srcMapPo mp) {
  assert(mp != Null);

  return mp->pcOffset;
}


// Use the JSON event parser to parse a source map file
// The format of the json is:
// { "pkg" : { "name" : pkgName, "version": version },
//   "map" : [
//      {"fun": {
//         "name": funName,
//         "arity": arity,
//         "lines" : [
//           { "line": lineNo,
//             "pc" : offset
//           }
//         ]
//      }, ..
//   ]
// }

static retCode startSrcMap(void *cl);
static retCode endSrcMap(void *cl);
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

JsonCallBacks mapEvents = {
  startSrcMap,
  endSrcMap,
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

typedef enum {
  initial,
  topLevel,
  inManifest,
  inPkgName,
  inPkgVers,
  inFun,
  inDetail,
  inMap
} ParseState;

static char *stNames[] = {"initial", "inManifest", "inPackage", "inDetail", "inResource"};

typedef struct {
  retCode status;
  char pkg[MAX_SYMB_LEN]; // Package name
  char ver[MAX_SYMB_LEN]; // Package version
  char fun[MAX_SYMB_LEN];
  integer arity;
  integer line;
  integer offset;
  ParseState state;
} ParsingState, *statePo;

retCode parseSrcMap(char *text, integer length) {
  bufferPo srcBuff = fixedStringBuffer(text, length);

  ParsingState info = {.state=initial, .status=Ok};
  yyparse(O_IO(srcBuff), &mapEvents, &info);
  return info.status;
}

retCode startSrcMap(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  if (tracePkg)
    logMsg(logFile, "Starting parse of source map");
  return Ok;
}

retCode endSrcMap(void *cl) {
  if (tracePkg)
    logMsg(logFile, "Ending parse of source map");
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest)
    logMsg(logFile, "Starting collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      info->state = topLevel;
      uniCpy((char *) &info->pkg, NumberOf(info->pkg), "");
      uniCpy((char *) &info->ver, NumberOf(info->ver), "");
      break;

    case inManifest:
      uniCpy((char *) &info->ver, NumberOf(info->ver), "");
      break;

    case inMap:



    default:
      return Error;
  }
  return Ok;
}

retCode endCollection(void *cl) {
  statePo info = (statePo) cl;

  if (tracePkg)
    logMsg(logFile, "Ending collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      return Error;
    case inManifest:
      info->state = topLevel;
      break;
    default:
      return Error;
  }

  return Ok;
}

retCode startArray(void *cl) {
  statePo info = (statePo) cl;
  switch (info->state) {
    case inMap:
      return Ok;
    default:
      return Error;
  }
}

retCode endArray(void *cl) {
  statePo info = (statePo) cl;
  switch (info->state) {
    case inMap:
      info->state = topLevel;
      return Ok;
    default:
      return Error;
  }
}

retCode startEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest)
    logMsg(logFile, "Starting entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case topLevel:
      if (uniCmp(name, "pkg") == same) {
        uniCpy((char *) &info->pkg, NumberOf(info->pkg), "");
        uniCpy((char *) &info->ver, NumberOf(info->ver), "");
        info->state = inManifest;
      } else if (uniCmp(name, "map") == same) {
        info->state = inMap;
      } else
        return Error;

    case inManifest: {
      if (uniCmp(name, "name") == same) {
        info->state = inPkgName;
      } else if (uniCmp(name, "vers") == same) {
        info->state = inPkgVers;
      } else
        return Error;
      break;
    }
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
    case inManifest:
    default:
      break;
  }

  return Ok;
}

retCode numEntry(double dx, void *cl) {
  statePo info = (statePo) cl;

  if (traceManifest)
    logMsg(logFile, "Numeric entry, state = %s, value=%g", stNames[info->state], dx);

  switch (info->state) {
    case inManifest:

    default:
      return Error;
  }
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
    case inPkgName:
      uniCpy((char *) &info->pkg, NumberOf(info->pkg), name);
      break;
    case inPkgVers:
      uniCpy((char *) &info->ver, NumberOf(info->ver), name);
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
