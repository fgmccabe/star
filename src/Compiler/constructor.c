/*
 * Constructor code management and generation
 */

#include "compiler.h"

#include "utils.h"
#include "compile.h"
#include "meta.h"
#include "codegen.h"

#include <hash.h>

/*
 * A constructor consists of a structure that looks like:
 * 
 * +--------+----------------+
 * | <code> | Data Cells ... |
 * +--------+----------------+
 *      \
 *       \
 *        ---------------------
 *                             \
 *                              \
 *                               v
 * +-------+-------+------+------+-----------+-----------+
 * | FTble | ConIx | Scav | Evac | This Code | BuildCode |
 * +-------+-------+------+------+-----------+-----------+
 *   |              /      |
 *   |    /--------/       |
 *   |   v                 |
 *   |  +--------------+   |
 *   |  | GC scavenge  |   v
 *   |  +--------------+   +--------------+
 *   v                     | GC Copy code |
 *   +-------------+       +--------------+
 *   | Field Table |
 *   +-------------+
 *
 * The ConIx is used during case analysis, it is an offset into the case
 * table. The G/C code has two parts: the evacuation code that copies the
 * structure into a new space and the scavenge code that updates pointer
 * references. The builder code acts like a normal function body.
 *
 * The Field table is a table of offsets of fields paired with hashs of the 
 * field names. This gives named access to the fields of the constructor
 *
 * Most of this structure is static compile-time code.  In order to support
 * forward pointers, there must be atleast enough space for a forward pointer.
 */

long countConstructors(lxPo defs)
{
  return sxLength(defs);
}

static retCode genConstructor(sxPo con,int conIx,int maxIx,
			      sxPo type,dictPo dict);
static retCode genEnumerated(sxPo con,int conIx,int maxIx,
			     sxPo type,dictPo dict);

retCode compileTypeDef(sxPo def,uniChar *path,dictPo dict)
{
  lxPo constructors = sxTypeDefCons(def);
  sxPo type = sxTypeDefType(def);

  int count = sxLength(constructors);
  retCode ret = Ok;
  int maxIx = count*POINTER_SIZE;

  for(int ix=0;ret==Ok && ix<count;ix++){
    sxPo conSpec = sxEl(constructors,ix);
    if(sxIsIden(conSpec))
      ret = genEnumerated(conSpec,ix*POINTER_SIZE,maxIx,type,dict);
    else if(sxIsConstructor(conSpec))
      ret = genConstructor(conSpec,ix*POINTER_SIZE,maxIx,type,dict);
    else
      reportError(sxLoc(conSpec),"%A is not a well defined constructor",conSpec);
  }

  return ret;
}

static lPo genEvacCode(mtdCxtPo mtd,int conIx,lPo entry,sxPo con);
static lPo genScavCode(mtdCxtPo mtd,int conIx,sxPo con);
static long sizeofConstructor(sxPo con);
static lPo genFieldTable(mtdCxtPo mtd,sxPo con);

retCode genConstructor(sxPo con,int conIx,int maxIx,sxPo type,dictPo dict)
{
  uniChar *op = sxCallOp(con);
  long conSize = sizeofConstructor(con);
  mtdCxtPo mtd = newMethod(op);
  assemPo code = methodCode(mtd);
  lPo conCode = newLbl(code,genSym(".L")); /* Pointer to constructor */
  lPo evac = genEvacCode(mtd,conIx,conCode,con); /* evacuation GC code */
  lPo scav = genScavCode(mtd,conIx,con); /* scavange gc code */
  lPo fields = genFieldTable(mtd,con); /* generate field accessing code */
  lxPo args = sxCallArgs(con);

  debug(outMsg(logFile,"constructor %A\n",con));
  AAlignTo(code,POINTER_SIZE);
  AConstP(code,fields);			/* Put in the field accessor */
  AConstI(code,conIx);			/* Put the constructor index in */
  AConstI(code,0);			/* Empty slot, no locals scanner */
  AConstP(code,scav);			/* Pointer to scavenger code */
  AConstP(code,evac);			/* Pointer to evacuation code */

  defineLbl(code,conCode);
  AConstS(code,op);			/* The name of this constructor */

  genMethodCode(mtd,conCode);		/* Generate the constructor code */

  declareConstructor(op,conIx,maxIx,conSize,args,
		     sxArrowType(sxLoc(con),args,type),
		     conCode,dict);
  return Ok;
}

/*
 * An enumerated symbol is constructed to be purely static
 */
retCode genEnumerated(sxPo con,int conIx,int maxIx,sxPo type,dictPo dict)
{
  uniChar *op = sxIden(con);
  mtdCxtPo mtd = newMethod(op);
  assemPo code = methodCode(mtd);
  lPo evac = newLbl(code,genSym(".E"));	/* special evac code for enums */
  lPo scav = newLbl(code,genSym(".S"));	/* should never be invoked */

  debug(outMsg(logFile,"enum %A\n",con));
  AAlignTo(code,POINTER_SIZE);
  AConstI(code,conIx);			/* Put the constructor index in */
  AConstI(code,0);			/* Empty slot, no locals scanner */
  AConstP(code,scav);			/* Pointer to scavenger code */
  AConstP(code,evac);			/* Pointer to evacuation code */

  lPo entryCode = currLbl(code,op);	/* entry to enumerated */
  AConstS(code,op);			/* for documentation purposes */

  AAlignTo(code,POINTER_SIZE);
  lPo conCode = currLbl(code,genSym(".L")); /* enumerated structure */
  AConstP(code,entryCode);		/* static closure structure */

  defineLbl(code,evac);
  AEnterCFun(code,0);
  ARetC(code,R1);

  defineLbl(code,scav);			/* Should never be called */
  AEnterCFun(code,0);
  ARetC(code,R1);

  genMethodCode(mtd,conCode);

  declareConstructor(op,conIx,maxIx,0,nil,sxArrowType(sxLoc(con),nil,type),conCode,dict);
  return Ok;
}

static long countFields(lxPo args)
{
  long count = 0;

  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(args,ix);

    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg)))
      count++;
  }
  return count;
}

static lPo genFieldTable(mtdCxtPo mtd,sxPo con)
{
  assemPo code = methodCode(mtd);
  lPo dotLabel = currLbl(code,genSym(".D"));
  lxPo args = sxCallArgs(con);

  long count = sxLength(args);
  long namedCount = countFields(args);	// How many have names?
  long offset = POINTER_SIZE;		/* offset to the field in question */

  AConstI(code,namedCount);		/* number of entries in table */

  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(args,ix);
    sxPo argType;

    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg))){
      argType = sxCastType(arg);

      AConstI(code,uniHash(sxIden(sxCastExp(arg))));
      AConstI(code,offset);
    }
    else argType = arg;

    if(isRawCharType(argType))
      offset += CHAR_SIZE;
      else if(isRawIntType(argType))
      offset += INTEGER_SIZE;
    else if(isRawLongType(argType))
      offset += LONG_SIZE;
    else if(isRawFloatType(argType))
      offset += DOUBLE_SIZE;
    else if(isRawStringType(argType))
      offset += POINTER_SIZE;
    else{
      offset += POINTER_SIZE;
    }
  }

  return dotLabel;			/* This is our field offset table */
}


/*
 * Generate the code that will be called during garbage collection.  There is
 * one argument: the constructor instance being collected. This ode copies the
 * constructor out to the new heap. Return the new location of the constructor
 */
lPo genEvacCode(mtdCxtPo mtd,int conIx,lPo entryPoint,sxPo con)
{
  assemPo code = methodCode(mtd);
  lPo fwdLabel = newLbl(code,genSym(".F")); /* The forwarding code that replaces */
  lPo gcLabel = currLbl(code,genSym(".G"));
  AEnterCFun(code,0);			/* R1 is the constructor */

  lPo failAlloc = newLbl(code,genSym(".gc"));
  AAllocH(code,R2,sizeofConstructor(con),failAlloc);
  AStLbl(code,R2,0,entryPoint);

  uniChar *currSeg = currSegment(code);
  setSegment(code,genSym(".gc"));
  defineLbl(code,failAlloc);		/* We start filling in the block */
  AHalt(code,99);
  setSegment(code,currSeg);

  long offset = POINTER_SIZE;
  lxPo args = sxCallArgs(con);

  long count = sxLength(args);
  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(args,ix);
    sxPo argType;
    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg)))
      argType = sxCastType(arg);
    else
      argType = arg;

    if(isRawCharType(argType)){
      ALdC(code,R3,R1,offset);		/* copy a character from old to new */
      AStC(code,R2,offset,R3);
      offset += CHAR_SIZE;
    } else if(isRawIntType(argType)){
      ALdI(code,R3,R1,offset);		/* copy an integer from old to new */
      AStI(code,R2,offset,R3);
      offset += INTEGER_SIZE;
    } else if(isRawLongType(argType)){
      ALdL(code,R3,R1,offset);		/* copy a long from old to new */
      AStL(code,R2,offset,R3);
      offset += LONG_SIZE;
    } else if(isRawFloatType(argType)){
      ALdD(code,FPR0,offset,R1);	/* copy a double from old to new */
      AStD(code,R2,offset,FPR0);
      offset += DOUBLE_SIZE;
    }
    else{
      ALd(code,R3,R1,offset);		/* copy other pointer */
      ASt(code,R2,offset,R3);
      offset += POINTER_SIZE;
    }
  }

  APlantFwd(code,R2,R1,fwdLabel);	/* Plant the forwarding code */
  ARetC(code,R2);			/* return the new constructor */

  AConstP(code,fwdLabel);		/* forwarding the forwarder */

  defineLbl(code,fwdLabel);
  AEnterCFun(code,0);			/* We will return new location */
  ALd(code,R0,R1,POINTER_SIZE);		/* return the forwarded pointer */
  ARetC(code,R0);
  
  return gcLabel;			/* This is our GC code */
}


/*
 * This code invokes the evacuation code for each pointer type in the
 * constructor. Return the address immediately after the constructor.  Called as
 * a regular cafe function, meaning that the constructor data shows up as the
 * environment of this function.
 */
lPo genScavCode(mtdCxtPo mtd,int conIx,sxPo con)
{
  assemPo code = methodCode(mtd);
  lPo gcLabel = currLbl(code,genSym(".L"));

  AEnterCFun(code,0);
  AMove(code,ENV,R1);

  long offset = POINTER_SIZE;
  lxPo args = sxCallArgs(con);

  long count = sxLength(args);
  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(args,ix);
    sxPo argType;

    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg)))
      argType = sxCastType(arg);
    else
      argType = arg;

    if(isRawCharType(argType))
      offset += CHAR_SIZE;
    else if(isRawIntType(argType))
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

  return gcLabel;			/* This is our GC code */
}

long sizeofConstructor(sxPo con)
{
  long size = POINTER_SIZE;
  lxPo args = sxCallArgs(con);

  long count = sxLength(args);
  for(int ix=0;ix<count;ix++){
    sxPo arg = sxEl(args,ix);
    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg))){
      sxPo argType = sxCastType(arg);
      size+=typeSize(argType);
    }
    else
      size+=typeSize(arg);
  }

  return size;
}

retCode compileConstructor(sxPo exp,sxPo *expected,
			   uniChar *path,
			   dictPo dict,dictPo outer,exitPo exit,
			   mtdCxtPo mtd,contFun cont,void *cl)
{
  assemPo code = methodCode(mtd);
  locationPo loc = sxLoc(exp);

  if(sxIsIden(exp)){
    uniChar *conName = sxIden(exp);
    conDefPo con = findConstructor(conName,dict);

    assert(con!=Null);

    if(checkType(arrowResType(con->type),expected,loc)!=Ok){
      reportError(loc,"%A not consistent with expected type: %T",exp,*expected);
      return Error;
    }

    if(sxLength(con->args)!=0){
      reportError(loc,"constructor %A should have %d args",
		  exp,sxLength(con->args));
      return Error;
    }
    else{
      VarInfoRec info = {.loc=loc,.where=label,.kind=general,.l.lit=con->lbl};
      return cont(loc,&info,cl,code);
    }
  }
  else if(sxIsConstructor(exp)){
    uniChar *conName = sxConstructorOp(exp);
    lxPo args = sxConstructorArgs(exp);
    conDefPo def = findConstructor(conName,dict);
    locationPo loc = sxLoc(exp);

    assert(def!=Null);

    sxPo conType = def->type;

    if(checkType(arrowResType(conType),expected,loc)!=Ok){
      reportError(loc,"%M not consistent with expected type: %T",exp,*expected);
      return Error;
    }

    int count = sxLength(args);

    if(count!=sxLength(def->args)){
      reportError(loc,"constructor %A should have %d args",
		  exp,sxLength(def->args));
      return Error;
    }
    else if(count==0){			/* enumerated symbols handled specially */
      VarInfoRec info = { .loc=loc,.where=label,.kind=general, .l.lit=def->lbl};
      return cont(loc,&info,cl,code);
    }
    else{
      VarInfoRec conInfo = { .loc=loc,.where=registr,.kind=general,.l.reg=R1};

      lPo allocClos = currLbl(code,genUSym(conName));
      lPo failAlloc = newLbl(code,genSym(".gc"));
      AAllocH(code,R1,def->conSize,failAlloc);
      AMove(code,TRM,R1);
      AStLbl(code,R1,0,def->lbl);

      uniChar *currSeg = currSegment(code);
      setSegment(code,genSym(".gc"));
      defineLbl(code,failAlloc);	/* We start filling in the block */
      AGc(code,def->conSize);
      gcCallSite(mtd,dict);		/* gc needs call site */
      ALd(code,ENV,FP,ENV_ARG_OFFSET);	/* reload the environment */
      AJmp(code,allocClos);		/* try again to allocate */
      setSegment(code,currSeg);

      long conOffset = POINTER_SIZE;	/* offset to the constructor data */
      retCode ret = Ok;
      int arity = sxLength(args);
      
      for(int ix=0;ret==Ok && ix<arity;ix++){ /* copy values into constructor */
	sxPo arg = sxEl(args,ix);
	sxPo argType = arrowtypeArg(conType,ix);
	ret = compileExp(arg,&argType,path,dict,outer,exit,mtd,copyToT,&conOffset);

	conOffset+=typeSize(argType);
      }

      if(ret==Ok)
	ret = cont(loc,&conInfo,cl,code);
      return ret;
    }
  }
  else{
    reportError(sxLoc(exp),"invalid constructor form: %A",exp);
    return Error;
  }
}
