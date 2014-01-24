/*
 * Garbage collection driver
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "compile.h"
#include "methodP.h"
#include "assemP.h"
#include "heapP.h"
#include <ooio.h>
#include <stdlib.h>
#include "type.h"

#undef debug
#ifdef TRACEASSM
#define debug(Op) if(debugAssem){ Op; }
#else
#define debug(Op)
#endif


HeapRecord heap,oldHeap;

static poolPo scanBlockPool;

void initHeap(long heapSize)
{
  if(scanBlockPool==Null){
    heap.curr = heap.old = heap.base = heap.start = malloc(heapSize); /* Allocate heap */
    heap.outerLimit = heap.base+heapSize;	/* The actual outer limit */
    heap.limit = heap.base+heapSize/2;
    heap.allocMode = lowerHalf;

    scanBlockPool = newPool(sizeof(GcScanBlock),256);

#ifdef MEMTRACE
    if(traceMemory){
      outMsg(logFile,"establish heap of %d bytes total\n",heapSize);
      outMsg(logFile,"lower half at 0x%x, %d bytes\n",heap.start,heap.limit-heap.base);
    }
#endif
  }
}

/*
 * Generate the GC copy out code for the closure. Leave behind a pointer to the
 * new structure.
 */
retCode genClosEvac(mtdPo mtd,lPo entryPoint,lPo evacLbl,lPo scanLbl,
		    lPo scavLbl, lxPo free,dictPo dict,int frSize)
{
  assemPo code = methodCode(mtd);
  lPo fwdLabel = newLbl(code,genSym(".F")); /* The forwarding code */
  defineLbl(code,evacLbl);		/* This is where we start */

  AEnterCFun(code,0);			/* R1 is the first argument */

  lPo failAlloc = newLbl(code,genSym(".h"));
  Register treg = R0;
  AAllocH(code,treg,frSize,failAlloc);
  AStLbl(code,treg,0,entryPoint);

  uniChar *currSeg = currSegment(code);
  setSegment(code,genSym(".gc"));
  defineLbl(code,failAlloc);		/* We start filling in the block */
  AHalt(code,99);			/* We are done if we need to GC here */

  setSegment(code,currSeg);

  long offset = POINTER_SIZE;

  long count = sxLength(free);
  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(free,ix);
    varInfoPo var = varReference(sxIden(arg),dict);
    sxPo argType = var->type;

    if(isRawIntType(argType)){
      ALdI(code,R3,R1,offset);
      AStI(code,treg,offset,R3);	/* copy an integer from old to new */
      offset += INTEGER_SIZE;
    } else if(isRawLongType(argType)){
      ALdL(code,R3,R1,offset);
      AStL(code,treg,offset,R3);	/* copy a long from old to new */
      offset += LONG_SIZE;
    } else if(isRawFloatType(argType)){
      ALdD(code,FPR1,R1,offset);
      AStD(code,treg,offset,FPR1);	/* copy a double from old to new */
      offset += DOUBLE_SIZE;
    } else if(isRawStringType(argType)){
      ALd(code,R3,R1,offset);
      ASt(code,treg,offset,R3);		/* copy string pointer */
      offset += POINTER_SIZE;
    }
    else{
      ALd(code,R3,R1,offset);
      ASt(code,treg,offset,R3);		/* copy other pointer */
      offset += POINTER_SIZE;
    }
  }

  APlantFwd(code,treg,R1,fwdLabel);	/* Plant the forwarding code */
  ARetC(code,treg);			/* return the new constructor */

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,scanLbl);		/* Scan, because we need */
  AConstP(code,scavLbl);		/* scavenge the closure */
  AConstP(code,fwdLabel);		/* forwarding the forwarder */

  defineLbl(code,fwdLabel);
  AEnterCFun(code,0);			/* We will return new location */
  ALd(code,R0,R1,POINTER_SIZE);		/* return the forwarded pointer */
  ARetC(code,R0);			/* return the new constructor */

  return Ok;				/* This is our GC code */
}

/*
 * This code invokes the evacuation code for each pointer type in the
 * closure. Return the address immediately after the closure.  Called as
 * a regular cafe function, meaning that the free data shows up as the
 * environment of this function.
 */
retCode genClosScav(mtdPo mtd,lPo scav,lxPo free,dictPo dict)
{
  assemPo code = methodCode(mtd);
  defineLbl(code,scav);

  AEnterCFun(code,0);
  AMove(code,ENV,R1);

  long offset = POINTER_SIZE;
  long count = sxLength(free);
  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(free,ix);
    varInfoPo var = varReference(sxIden(arg),dict);
    sxPo argType = var->type;

    if(isRawIntType(argType))
      offset += INTEGER_SIZE;
    else if(isRawLongType(argType))
      offset += LONG_SIZE;
    else if(isRawFloatType(argType))
      offset += DOUBLE_SIZE;
    else if(isRawStringType(argType))
      offset += POINTER_SIZE;
    else{
      AEvac(code,ENV,offset);		/* Call the copy code for this */
      offset += POINTER_SIZE;
    }
  }
  
  /* return the address of the next element in the heap */
  
  ARetNext(code,ENV,offset);
  return Ok;
}

/*
 * Generate scanners for the locals a function
 */

static gcScanPo newScanRecord(mtdPo mtd,lPo callSite,listPo references)
{
  assemPo code = methodCode(mtd);
  gcScanPo blocks = mtd->scanBlocks;
  while(blocks!=Null){
    if(sameLists(blocks->references,references)){
      gcScanPo scan = (gcScanPo)allocPool(scanBlockPool);
      scan->callSite = callSite;
      scan->scanCode = blocks->scanCode;
      scan->references = references;
      scan->next = mtd->scanBlocks;
      mtd->scanBlocks = scan;
      return scan;
    }
    else
      blocks = blocks->next;
  }
  gcScanPo scan = (gcScanPo)allocPool(scanBlockPool);
  scan->callSite = callSite;
  scan->scanCode = newLbl(code,genSym(".S"));
  scan->references = references;
  scan->next = mtd->scanBlocks;
  mtd->scanBlocks = scan;
  return scan;
}

typedef struct {
  mtdPo mtd;
  listPo references;
} VrGcListRecord, *vrGcPo;

static retCode vrGcCheck(uniChar *name,varInfoPo var,void *cl)
{
  if(isVrLocal(var) && !isRawType(vrInfType(var))){
    vrGcPo info = (vrGcPo)cl;
    info->references = cons(var,info->references);
  }
  return Ok;
}

void gcCallSite(mtdPo mtd,dictPo dict)
{
  VrGcListRecord info = {mtd,emptyList};

  processDict(dict,vrGcCheck,&info);

  newScanRecord(mtd,currLbl(methodCode(mtd),genSym(".L")),info.references);
}

void genLocalScanner(lPo scanTable,dictPo dict,mtdPo mtd)
{
  assemPo code = methodCode(mtd);
  gcScanPo block = mtd->scanBlocks;

  while(block!=Null){
    listPo refs = block->references;
    if(!labelDefined(block->scanCode)){
      defineLbl(code,block->scanCode);
      AEnterCFun(code,0);
      AMove(code,TRM,R1);		/* R1 has the first argument */
      
      while(refs!=Null){
	varInfoPo var = (varInfoPo)head(refs);
	AEvac(code,TRM,var->l.off);
	refs = tail(refs);
      }
      ARtnC(code);
    }
    block = block->next;
  }
  AAlignTo(code,sizeof(void*));		/* make sure the table is aligned */
  defineLbl(code,scanTable);		/* We built the scanner table */
  block = mtd->scanBlocks;
  while(block!=Null){
    AConstP(code,block->callSite);	/* Each entry has the call site */
    AConstP(code,block->scanCode);	/* and the scanner code */
    block = block->next;
  }
}

static inline codeStructPo toCode(envPo env)
{
  return env->code-1;			/* The code is always past the table */
}

static inline logical inHeap(heapPo h,void *p)
{
  return p>=h->start && p<h->curr;
}

void scanStack(framePo fp,void *rtn)
{
  envPo env = fp->env;

  while(env!=Null){
    codeStructPo code = toCode(env);
    scanTablePo table = code->scanners;

    fp->env = code->evacuator(env);	/* We copy out the closure itself */

    while(True){
      if(table->rtn == rtn){
	if(table->scanner!=Null)
	  table->scanner(fp);		/* Scan the locals */
	break;
      }
      else
	table++;			/* look at the next entry */
    }

    rtn = fp->rtn;
    fp = fp->fp;
    env = fp->env;			/* look at saved closure */
  }
}

void scanHeap()
{
  envPo closure = (envPo)heap.start;	/* We now trawl the heap */

  while((void*)closure<heap.curr){
    codeStructPo code = toCode(closure);
    closure = code->scavenger(closure);
  }
}

void collect(framePo stack,void *site,int amnt)
{
#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"starting gc\n");
#endif

  oldHeap = heap;			/* copy current settings */
  switch(heap.allocMode){
  case lowerHalf:
#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"switching to upper half\n");
#endif
    heap.start = heap.curr;
    heap.limit = heap.outerLimit;	/* shift to the upper half */
    heap.allocMode = upperHalf;		/* It is guaranteed to have enough room */
    break;
  case upperHalf:			/* Shift to the lower half */
#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"switching to lower half\n");
#endif
    heap.limit = heap.start;
    heap.start = heap.base;
    heap.curr = heap.base;
    heap.allocMode = lowerHalf;
  default:
    ;
  }

#ifdef MEMTRACE
  if(traceMemory)
    outMsg(logFile,"%d bytes in available heap\n",heap.limit-heap.curr);
#endif

  scanStack(stack,site);			/* Scan the stack for roots */
  scanHeap();

  if(heap.curr+amnt>=heap.limit){
    logMsg(logFile,"heap exhausted");
    exit(99);
  }
#ifdef MEMTRACE
  if(traceMemory){
    outMsg(logFile,"%d bytes used\n",heap.curr-heap.start);
    outMsg(logFile,"%d bytes available\n",heap.limit-heap.curr);
}
#endif

}

