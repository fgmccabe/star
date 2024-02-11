#include "config.h"
#include <ooio.h>
#include <turm.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include <tpl.h>
#include <cons.h>
#include <option.h>
#include "ideal.h"
#include "labelsP.h"
#include "globalsP.h"
#include "vectP.h"

termPo eOk;
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
termPo divZero;
termPo noValue;
termPo hasValue;

termPo falseEnum;
termPo trueEnum;
termPo voidEnum;
termPo eofEnum;
termPo canceledEnum;
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
static termPo glbFinalizer(specialClassPo class, termPo o);

SpecialClass GlobalClass = {
  .clss = Null,
  .sizeFun = glbSize,
  .copyFun = glbCopy,
  .scanFun = glbScan,
  .finalizer = glbFinalizer,
  .compFun = glbCmp,
  .hashFun = glbHash,
  .dispFun = glbDisp
};

clssPo globalClass = (clssPo) &GlobalClass;

void initGlobals() {
  GlobalClass.clss = specialClass;
  globals = newHash(1024, (hashFun) globalHash, (compFun) globalCmp, (destFun) globalDel);

  glbVars = (globalPo) malloc(sizeof(GlobalRecord) * 1024);
  glbVarTblSize = 1024;
  numGlbVars = 0;

  eOk = declareEnum("star.core#Ok", -1, globalHeap);
  eINTRUPT = declareEnum("star.core#eINTRUPT", -1, globalHeap);
  eNOTDIR = declareEnum("star.core#eNOTDIR", -1, globalHeap);
  eNOFILE = declareEnum("star.core#eNOFILE", -1, globalHeap);
  eNOTFND = declareEnum("star.core#eNOTFND", -1, globalHeap);
  eINVAL = declareEnum("star.core#eINVAL", -1, globalHeap);
  eRANGE = declareEnum("star.core#eRANGE", -1, globalHeap);
  eNOPERM = declareEnum("star.core#eNOPERM", -1, globalHeap);
  eFAIL = declareEnum("star.core#eFAIL", -1, globalHeap);
  eIOERROR = declareEnum("star.core#eIOERROR", -1, globalHeap);
  eCONNECT = declareEnum("star.core#eCONNECT", -1, globalHeap);
  eDEAD = declareEnum("star.core#eDEAD", -1, globalHeap);

  falseEnum = declareEnum("star.core#false", -1, globalHeap);
  trueEnum = declareEnum("star.core#true", -1, globalHeap);

  voidEnum = declareEnum("star.core#void", -1, globalHeap);

  eofEnum = declareEnum("star.core#eof", -1, globalHeap);
  canceledEnum = declareEnum("star.core#canceled", -1, globalHeap);

  errorLbl = declareLbl("star.core#error", 2, -1);

  divZero = declareEnum("star.core#divZero", -1, globalHeap);

  noValue = declareEnum("star.core#noValue", -1, globalHeap);
  hasValue = declareEnum("star.core#hasValue", -1, globalHeap);

  unitEnum = (termPo) allocateTpl(globalHeap, 0);
}

globalPo C_GLOB(termPo t) {
  assert(hasClass(t, globalClass));

  return (globalPo) t;
}

globalPo globalVar(const char *nm) {
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
    glb->clss = globalClass;
    glb->hash = tst.hash;
    glb->varNo = numGlbVars - 1;
    hashPut(globals, glb, glb);
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

  return o + GlobalCellCount;
}

termPo glbFinalizer(specialClassPo class, termPo o) {
  globalPo glb = C_GLOB(o);

  return o + GlobalCellCount;
}

static void markGlobal(globalPo glb, gcSupportPo G) {
  if (glb->content != Null)
    glb->content = markPtr(G, (ptrPo) &glb->content);
}

void markGlobals(gcSupportPo G) {
  for (int32 ix = 0; ix < numGlbVars; ix++)
    markGlobal(&glbVars[ix], G);

  eOk = markPtr(G, &eOk);
  eINTRUPT = markPtr(G, &eINTRUPT);
  eNOTDIR = markPtr(G, &eNOTDIR);
  eNOFILE = markPtr(G, &eNOFILE);
  eNOTFND = markPtr(G, &eNOTFND);
  eINVAL = markPtr(G, &eINVAL);
  eRANGE = markPtr(G, &eRANGE);
  eNOPERM = markPtr(G, &eNOPERM);
  eFAIL = markPtr(G, &eFAIL);
  eIOERROR = markPtr(G, &eIOERROR);
  eCONNECT = markPtr(G, &eCONNECT);
  eDEAD = markPtr(G, &eDEAD);

  falseEnum = markPtr(G, &falseEnum);
  trueEnum = markPtr(G, &trueEnum);

  voidEnum = markPtr(G, &voidEnum);
  eofEnum = markPtr(G, &eofEnum);
  canceledEnum = markPtr(G, &canceledEnum);

  nilEnum = markPtr(G, &nilEnum);
  noneEnum = markPtr(G, &noneEnum);

  divZero = markPtr(G, &divZero);

  noValue = markPtr(G, &noValue);
  hasValue = markPtr(G, &hasValue);

  unitEnum = markPtr(G, (ptrPo) &unitEnum);

  hNilEnum = markPtr(G, &hNilEnum);
  scanVect(G);
}

retCode glbDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  globalPo glb = C_GLOB(t);

  if (getGlobal(glb) != Null)
    return outMsg(out, "global: %s=%,*T", glb->name, depth, glb->content);
  else
    return outMsg(out, "global: %s (undef)", glb->name);
}

termPo getGlobal(globalPo v) {
  return v->content;
}

char *globalVarName(globalPo v) {
  return v->name;
}

globalPo findGlobalVar(int32 varNo) {
  return &glbVars[varNo];
}

int32 globalVarNo(const char *nm) {
  return (int32) (globalVar(nm)->varNo);
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

termPo ioErrorCode(retCode ret) {
  switch (ret) {
    case Ok:
      return voidEnum;
    case Error:
      return eIOERROR;
    case Eof:
      return eofEnum;
    default:
      return eIOERROR;
  }
}
