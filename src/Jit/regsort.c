//
// Created by Francis McCabe on 9/25/25.
//

#include "sort.h"
#include "assert.h"
#include "ooio.h"

typedef struct
{
  int32 top;
  int32 size;
  argSpecPo* stack;
} Stack, *stkPo;

static int32 stackCount(stkPo stack)
{
  return stack->top;
}

static argSpecPo stackPeek(stkPo stack, int32 ix)
{
  if (ix >= stack->top)
    return Null;
  return stack->stack[ix];
}

static argSpecPo stackPop(stkPo stack)
{
  if (stack->top == 0)
    return Null;
  return stack->stack[--stack->top];
}

static void stackPush(argSpecPo spec, stkPo stack)
{
  assert(stack->top < stack->size);
  stack->stack[stack->top++] = spec;
}

static argSpecPo nextDef(ArgSpec defs[], int32 arity)
{
  for (int32 ix = 0; ix < arity; ix++){
    argSpecPo def = &defs[ix];
    if (def->mark){
      return def;
    }
  }
  return Null;
}


static int32 analyseDef(argSpecPo def,
                        ArgSpec defs[],
                        int32 arity,
                        stkPo stack,
                        int32* groups);

static int32 analyseRef(argSpecPo ref,
                        ArgSpec defs[],
                        int32 arity,
                        stkPo stack,
                        int32* groups,
                        int32 low)
{
  // Is this reference already in the stack?
  for (int32 ix = stackCount(stack); ix > 0; ix--){
    argSpecPo stkRef = stackPeek(stack, ix - 1);

    if (affects(ref->dst, stkRef->src))
      return min(low, ix);
  }
  // look in definitions

  if (ref->mark)
    return min(low, analyseDef(ref, defs, arity, stack, groups));
  else
    return low;
}

argSpecPo findRef(argSpecPo def, ArgSpec defs[], int32 arity)
{
  for (int32 ix = 0; ix < arity; ix++){
    argSpecPo candidate = &defs[ix];
    if (candidate->mark && affects(candidate->dst, def->src)){
      return &defs[ix];
    }
  }

  return Null;
}

int32 analyseDef(argSpecPo def,
                 ArgSpec defs[],
                 int32 arity,
                 stkPo stack,
                 int32* groups)
{
  int32 pt = stackCount(stack);
  stackPush(def, stack);
  def->mark = False;

  // Is this reference already in the stack?
  for (int32 ix = pt; ix > 0; ix--){
    // We dont need to check the definition we just pushed
    argSpecPo stkRef = stackPeek(stack, ix-1);

    if (affects(stkRef->dst, def->src))
      return ix-1;
  }

  int low = pt;
  argSpecPo ref;
  while ((ref = findRef(def, defs, arity)) != Null){
    low = min(low, analyseDef(ref, defs, arity, stack, groups));
  }

  if (low < stackCount(stack)){
    int32 group = (*groups)++;

    while (low < stackCount(stack)){
      argSpecPo spec = stackPop(stack);
      spec->group = group;
    }
  }
  return low;
}

static void showGroups(ArgSpec defs[], int32 groups, int32 arity)
{
  for (int32 gx = 0; gx < groups; gx++){
    outMsg(logFile, "group %d: ", gx);
    char* sep = "";
    for (int32 ax = 0; ax < arity; ax++){
      if (defs[ax].group == gx){
        outMsg(logFile, "%s%F <- %F", sep, defs[ax].dst, defs[ax].src);
        sep = ", ";
      }
    }
    outStr(logFile, "\n");
  }
  flushOut();
}

static void showDefs(ArgSpec defs[], int32 count)
{
  char* sep = "";
  for (int32 ax = 0; ax < count; ax++){
    argSpecPo arg = &defs[ax];
    outMsg(logFile,"%s%F <- %F",sep,arg->dst,arg->src);
    sep = ", ";
  }
  outStr(logFile, "\n");
  flushOut();
}

int32 sortSpecs(ArgSpec defs[], int32 arity)
{
  int32 groups = 0;
  argSpecPo stackData[arity];
  Stack stack = {.top = 0, .stack = stackData, .size = arity};

#ifdef TRACEJIT
  if (traceJit >= detailedTracing){
    showDefs(defs, arity);
  }
#endif

  argSpecPo def;
  while ((def = nextDef(defs, arity)) != Null){
    analyseDef(def, defs, arity, &stack, &groups);
  }

#ifdef TRACEJIT
  if (traceJit >= detailedTracing){
    showGroups(defs, groups, arity);
  }
#endif

  assert(stack.top==0);
  return groups;
}

// This is horrendous, but does not matter
int32 groupSize(argSpecPo specs, int32 arity, int32 group)
{
  int32 size = 0;
  for (int32 ix = 0; ix < arity; ix++){
    if (specs[ix].group == group)
      size++;
  }
  return size;
}
