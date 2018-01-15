/*
 * Ancilliary functions to support the generation of memo functions
 */
#include "compiler.h"
#include "dict.h"
#include "meta.h"
#include "compile.h"
#include "package.h"
#include "codegen.h"
#include "memo.h"
#include <iostr.h>

//
// A K function is a constant function. 
// K(X) is (function() is X)
// A memo function is built to mimic this function, specialized as needed
// to specific types.
//
lPo findKFun(locationPo loc,sxPo type)
{
  char buffer[1024];
  sourceKind kind = typeRep(type);
  char *kName = strMsg(buffer,NumberOf(buffer),".K%s",kindName(kind));

  mtdCxtPo kMtd = newMethod(NULL, kName, 0);
  assemPo code = methodCode(kMtd);

  lPo catch = newLbl(code,genSym(".C"));
  lPo scan = newLbl(code,genSym(".S"));
  lPo evacLbl = newLbl(code,genSym(".E"));
  lPo scavLbl = newLbl(code,genSym(".V"));

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,catch);
  AConstP(code,scan);			/* locals scanner */
  AConstP(code,scavLbl);
  AConstP(code,evacLbl);		/* Must be in this order */

  lPo entryPoint = currLbl(code,genSym(".K"));
    
  AEnterFun(code,0);			/* No locals */

  VarInfoRec valSrc = { .kind = kind, .where=basedVar, .base=ENV,
			.l.off=POINTER_SIZE };
  returnCont(loc,&valSrc,Null,code);	/* This is the K part */

  defineLbl(code,scan);			/* This will never be used */

  defineLbl(code,evacLbl);		/* Set up the evacuation code */
  AEnterCFun(code,0);			/* Evac is called as a C function */
  
  lPo failAlloc = newLbl(code,genSym(".h"));
  lPo fwdLabel = newLbl(code,genSym(".F")); /* The forwarding code */
  int frSize = POINTER_SIZE+sourceSize(kind);
  Register treg = ENV;

  AAllocH(code,treg,frSize,failAlloc);
  AStLbl(code,treg,0,entryPoint);

  VarInfoRec orgSrc = { .kind=kind, .where=basedVar, .base=R1,
			.l.off=POINTER_SIZE };
  VarInfoRec tgtSrc = { .kind=kind, .where=basedVar, .base=treg,
			.l.off=POINTER_SIZE };

  assignVar(loc,&orgSrc,&tgtSrc,code);	/* Copy the value out */
  APlantFwd(code,treg,R1,fwdLabel);	/* plant forwarding code */
  ARetC(code,treg);			/* We are done with the evac */

  AAlignTo(code,POINTER_SIZE);
  AConstP(code,scan);			/* Scan, because we need */
  AConstP(code,scavLbl);		/* scavenge the closure */
  AConstP(code,fwdLabel);		/* forwarding the forwarder */

  defineLbl(code,fwdLabel);
  AEnterCFun(code,0);			/* We will return new location */
  ALd(code,R0,R1,POINTER_SIZE);		/* return the forwarded pointer */
  ARetC(code,R0);			/* return the new constructor */

  // Implement the scavenger
  defineLbl(code,scavLbl);
  AEnterCFun(code,0);			/* Scavengers are also C called */
  AMove(code,ENV,R1);

  if(!isRawType(type))			/* We evacuate non-raw types */
    AEvac(code,ENV,POINTER_SIZE);

  ARetNext(code,ENV,frSize);		/* return a pointer to next object */
  
  genMethodCode(kMtd,entryPoint);	/* Generate the code */
  return entryPoint;
}
