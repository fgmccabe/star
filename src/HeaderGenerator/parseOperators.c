//
// Created by Francis McCabe on 5/22/18.
//

#include <ooio.h>
#include "parseOperators.h"
#include "genoperators.h"
#include "trie.h"

logical traceParse = False;

static retCode startTable(void *cl);
static retCode endTable(void *cl);
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

JsonCallBacks operEvents = {
  startTable,
  endTable,
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
  inDecl,
  inOper,
  inToken,
  inPriorities,
  inDescription,
  inBkt,
  inLeft,
  inRight,
  inPriority,
  unknownState
} ParseState;

static char *stNames[] = {"initial", "inDecl", "inOper", "token", "priorities", "desc", "bracket", "left", "right",
                          "priority", "unknown"};

static OperatorStyle operatorMode(const char *name);
static ParseState fromStName(const char *name);

typedef struct {
  char oper[1024]; // Package name
  OperatorStyle mode;
  int priorities[4];
  int numPriorities;
  integer priority;
  char left[16];
  char right[16];
  char desc[1024];
  ParseState state;
} ParsingState, *statePo;

retCode startTable(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  if (traceParse)
    logMsg(logFile, "Starting parse of operators");
  return Ok;
}

retCode endTable(void *cl) {
  if (traceParse)
    logMsg(logFile, "Ending parse of operators");
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Starting collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      info->state = inDecl;
      uniCpy((char *) &info->oper, NumberOf(info->oper), "");
      info->numPriorities = 0;
      break;

    default:
      return Error;
  }
  return Ok;
}

retCode endCollection(void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Ending collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      return Error;
    case inDecl:
      info->state = initial;
      switch (info->mode) {
        case infixOp:
          genInfix(info->oper, info->priorities[0], info->priorities[1], info->priorities[2], info->desc);
          return Ok;
        case prefixOp:
          genPrefix(info->oper, info->priorities[0], info->priorities[1], info->desc);
          return Ok;
        case postfixOp:
          genPostfix(info->oper, info->priorities[0], info->priorities[1], info->desc);
          return Ok;
        case brackets:
          genBracket(info->oper, info->priority, info->left, info->right, info->desc);
          return Ok;
        case tokenOnly:
          genToken(info->oper, info->desc);
          return Ok;
        default:
          return Error;
      }
      break;
    default:
      return Error;
  }

  return Ok;
}

retCode startArray(void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Starting array, state = %s", stNames[info->state]);
  return Ok;
}

retCode endArray(void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Ending array, state = %s", stNames[info->state]);
  return Ok;
}

retCode startEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Starting entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case inDecl: {
      info->state = fromStName(name);

      switch (info->state) {
        case inOper:
          info->mode = operatorMode(name);
          return Ok;
        case inPriorities:
          info->numPriorities = 0;
          return Ok;
        case inDescription:
          return Ok;
        case inToken:
          info->mode = tokenOnly;
          return Ok;
        case inBkt:
          info->mode = brackets;
          return Ok;
        default:
          return Error;
      }
    }
    default:
      return Error;
  }
}

retCode endEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Ending entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case inOper:
    case inPriorities:
    case inDescription:
    case inToken:
      info->state = inDecl;
      return Ok;
    case inLeft:
    case inRight:
    case inPriority:
    case inBkt:
      info->state = inDecl;
      return Ok;

    default:
      return Error;
  }
}

retCode numEntry(double dx, void *cl) {
  statePo info = (statePo) cl;
  if (traceParse)
    logMsg(logFile, "Numeric entry, value=%d", stNames[info->state], dx);

  switch (info->state) {
    case inPriorities: {
      if (info->numPriorities >= NumberOf(info->priorities))
        return Error;
      else {
        info->priorities[info->numPriorities++] = (int) dx;
        return Ok;
      }
    }
    case inPriority:
      info->priority = (integer) dx;
      return Ok;
    default:
      return Error;

  }
}

retCode boolEntry(logical trueVal, void *cl) {
  return Ok;
}

retCode txtEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  if (traceParse)
    logMsg(logFile, "Text entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case inOper:
      uniCpy(info->oper, NumberOf(info->oper), name);
      return Ok;
    case inLeft:
      uniCpy(info->left, NumberOf(info->left), name);
      return Ok;
    case inRight:
      uniCpy(info->right, NumberOf(info->right), name);
      return Ok;
    case inDescription:
      uniCpy(info->desc, NumberOf(info->desc), name);
      return Ok;
    case inToken:
      uniCpy(info->oper, NumberOf(info->oper), name);
      return Ok;
    case inBkt:
      uniCpy(info->oper, NumberOf(info->oper), name);
      return Ok;
    default:
      return Error;
  }
}

retCode nullEntry(void *cl) {
  return Ok;
}

retCode errorEntry(const char *name, void *cl) {
  logMsg(logFile, "Error: %s", name);
  return Ok;
}

static char *opModeNames[] = {
  "token",
  "infixOp",
  "prefixOp",
  "postfixOp",
  "bracket"};

OperatorStyle operatorMode(const char *name) {
  for (int ix = 0; ix < NumberOf(opModeNames); ix++) {
    if (uniCmp(name, opModeNames[ix]) == same)
      return (OperatorStyle) ix;
  }
  return unknownOperatorStyle;
}

ParseState fromStName(const char *name) {
  for (int ix = 0; ix < NumberOf(stNames); ix++) {
    if (uniCmp(name, stNames[ix]) == same)
      return (ParseState) ix;
  }
  if (operatorMode(name) != unknownOperatorStyle)
    return inOper;

  return unknownState;
}

retCode parseOperators(char *fname) {
  ioPo inFile = openInFile(fname, utf8Encoding);

  if (inFile != NULL) {
    ParsingState info = {.state=initial};
    yyparse(inFile, &operEvents, &info);
    return Ok;
  } else {
    return Fail;
  }
}

