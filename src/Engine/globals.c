#include "config.h"
#include <ooio.h>
#include <turm.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include <tpl.h>
#include "labels.h"
#include "globalsP.h"
#include "errorCodes.h"

termPo eINTRUPT;
termPo eINVAL;
termPo eRANGE;
termPo eNOFILE;
termPo eNOTDIR;
termPo eNOTFND;
termPo eNOPERM;
termPo eIOERROR;
termPo eCONNECT;
termPo eFAIL;
termPo eDEAD;

termPo falseEnum;
termPo trueEnum;
termPo voidEnum;
termPo okEnum;
termPo failEnum;
termPo eofEnum;
termPo unitEnum;
labelPo errorLbl;

static hashPo globals;

static GlobalRecord *glbVars;
static int32 numGlbVars;
static int32 glbVarTblSize;

static integer globalHash(globalPo glb);
static comparison globalCmp(globalPo lb1, globalPo lb2);
static retCode globalDel(globalPo glb, globalPo l);

static long glbSize(specialClassPo cl, termPo o);
static termPo glbCopy(specialClassPo cl, termPo dst, termPo src);
static termPo glbScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical glbCmp(specialClassPo cl, termPo o1, termPo o2);
static integer glbHash(specialClassPo cl, termPo o);
static retCode glbDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass GlobalClass = {
  .clss = Null,
  .sizeFun = glbSize,
  .copyFun = glbCopy,
  .scanFun = glbScan,
  .compFun = glbCmp,
  .hashFun = glbHash,
  .dispFun = glbDisp
};

clssPo globalClass = (clssPo) &GlobalClass;

void initGlobals() {
  GlobalClass.clss = specialClass;
  globals = NewHash(1024, (hashFun) globalHash, (compFun) globalCmp, (destFun) globalDel);

  glbVars = (globalPo) malloc(sizeof(GlobalRecord) * 1024);
  glbVarTblSize = 1024;
  numGlbVars = 0;

  eINTRUPT = (termPo) declareEnum("star.core#eINTRUPT");
  eNOTDIR = (termPo) declareEnum("star.core#eNOTDIR");
  eNOFILE = (termPo) declareEnum("star.core#eNOFILE");
  eNOTFND = (termPo) declareEnum("star.core#eNOTFND");
  eINVAL = (termPo) declareEnum("star.core#eINVAL");
  eRANGE = (termPo) declareEnum("star.core#eRANGE");
  eNOPERM = (termPo) declareEnum("star.core#eNOPERM");
  eFAIL = (termPo) declareEnum("star.core#eFAIL");
  eIOERROR = (termPo) declareEnum("star.core#eIOERROR");
  eCONNECT = (termPo) declareEnum("star.core#eCONNECT");
  eDEAD = (termPo) declareEnum("star.core#eDEAD");

  falseEnum = (termPo) declareEnum("star.core#false");
  trueEnum = (termPo) declareEnum("star.core#true");

  voidEnum = (termPo) declareEnum("star.core#void");

  okEnum = (termPo) declareEnum("star.core#ok");
  failEnum = (termPo) declareEnum("star.core#fail");
  eofEnum = (termPo) declareEnum("star.core#eof");

  errorLbl = declareLbl("star.core#error", 2);

  unitEnum = (termPo) allocateTpl(currHeap, 0);
}

globalPo C_GLOB(termPo t) {
  assert(hasClass(t, globalClass));

  return (globalPo) t;
}

globalPo globalVar(const char *nm, termPo provider) {
  GlobalRecord tst = {.name=(char *) nm, .hash=uniHash(nm)};
  globalPo glb = hashGet(globals, &tst);

  if (glb == Null) {
    if (numGlbVars >= glbVarTblSize) {
      int32 newTblSize = glbVarTblSize * 2;
      globalPo newTbl = (globalPo) malloc(sizeof(GlobalRecord) * newTblSize);
      memcpy(newTbl, glbVars, sizeof(GlobalRecord) * numGlbVars);
      free(glbVars);
      glbVars = newTbl;
      glbVarTblSize = newTblSize;
    }

    glb = (globalPo) &glbVars[numGlbVars++];

    glb->name = uniDuplicate(nm);
    glb->content = Null;
    glb->provider = provider;
    glb->clss = globalClass;
    glb->hash = tst.hash;
    glb->varNo = numGlbVars - 1;
    hashPut(globals, glb, glb);
  } else if (provider != Null && glb->provider == Null) {
    glb->provider = provider;
  }
  return glb;
}

long glbSize(specialClassPo cl, termPo o) {
  return GlobalCellCount;
}

integer globalHash(globalPo glb) {
  return glb->hash;
}

comparison globalCmp(globalPo lb1, globalPo lb2) {
  return uniCmp(lb1->name, lb2->name);
}

logical glbCmp(specialClassPo cl, termPo o1, termPo o2) {
  globalPo i1 = C_GLOB(o1);
  globalPo i2 = C_GLOB(o2);

  return (logical) (globalCmp(i1, i2) == same);
}

static integer glbHash(specialClassPo cl, termPo o) {
  globalPo glb = C_GLOB(o);
  return globalHash(glb);
}

retCode globalDel(globalPo glb, globalPo l) {
  uniDestroy(glb->name);
  glb->name = Null;
  glb->content = Null;
  return Ok;
}

termPo glbCopy(specialClassPo cl, termPo dst, termPo src) {
  *((globalPo) dst) = *((globalPo) src);
  return dst + GlobalCellCount;
}

termPo glbScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  globalPo glb = C_GLOB(o);

  if (glb->content != Null)
    helper((ptrPo) (&glb->content), c);

  if (glb->provider != Null)
    helper((ptrPo) (&glb->provider), c);

  return o + GlobalCellCount;
}

static void markGlobal(globalPo glb, gcSupportPo G) {
  if (glb->content != Null)
    glb->content = markPtr(G, (ptrPo) &glb->content);
  if (glb->provider != Null)
    glb->provider = markPtr(G, (ptrPo) &glb->provider);
}

void markGlobals(gcSupportPo G) {
  for (int32 ix = 0; ix < numGlbVars; ix++)
    markGlobal(&glbVars[ix], G);
  unitEnum = markPtr(G, (ptrPo) &unitEnum);
}

retCode glbDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  globalPo glb = C_GLOB(t);

  if (getGlobal(glb) != Null)
    return outMsg(out, "global: %s=%t", glb->name, glb->content);
  else
    return outMsg(out, "global: %s (undef)", glb->name);
}

termPo getGlobal(globalPo v) {
  return v->content;
}

globalPo getGlobalVar(char *nm) {
  return globalVar(nm, Null);
}

char *globalVarName(globalPo v) {
  return v->name;
}

globalPo findGlobalVar(int32 varNo) {
  return &glbVars[varNo];
}

int32 globalVarNo(const char *nm) {
  return (int32) (globalVar(nm, NULL)->varNo);
}

logical isValidGlobalVarNo(int32 varNo) {
  if (varNo >= 0 && varNo < numGlbVars) {
    return True;
  } else
    return False;
}

logical glbIsSet(globalPo glb) {
  return (logical) (glb->content != Null);
}

termPo setGlobalVar(globalPo v, termPo e) {
  assert(e != Null);

  termPo prev = v->content;
  v->content = e;
  return prev;
}

logical glbHasProvider(globalPo glb) {
  return (logical) (glb->provider != Null);
}

termPo getProvider(globalPo glb) {
  return glb->provider;
}
