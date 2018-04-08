#include "config.h"
#include <ooio.h>
#include <turm.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>
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
labelPo errorLbl;
labelPo locLbl;

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
static comparison glbCmp(specialClassPo cl, termPo o1, termPo o2);
static integer glbHash(specialClassPo cl, termPo o);
static retCode glbDisp(ioPo out, termPo t, long depth, logical alt);

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

  eINTRUPT = (termPo) declareEnum("eINTRUPT");
  eNOTDIR = (termPo) declareEnum("eNOTDIR");
  eNOFILE = (termPo) declareEnum("eNOFILE");
  eNOTFND = (termPo) declareEnum("eNOTFND");
  eINVAL = (termPo) declareEnum("eINVAL");
  eRANGE = (termPo) declareEnum("eRANGE");
  eNOPERM = (termPo) declareEnum("eNOPERM");
  eFAIL = (termPo) declareEnum("eFAIL");
  eIOERROR = (termPo) declareEnum("eIOERROR");
  eCONNECT = (termPo) declareEnum("eCONNECT");
  eDEAD = (termPo) declareEnum("eDEAD");

  falseEnum = (termPo) declareEnum("core.star#false");
  trueEnum = (termPo) declareEnum("core.star#true");

  voidEnum = (termPo) declareEnum("code.star#void");

  okEnum = (termPo) declareEnum("code.star#ok");
  failEnum = (termPo) declareEnum("code.star#fail");
  eofEnum = (termPo) declareEnum("code.star#eof");
  errorLbl = declareLbl("code.star#error", 2);
  locLbl = declareLbl("loc", 5);
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

comparison glbCmp(specialClassPo cl, termPo o1, termPo o2) {
  globalPo i1 = C_GLOB(o1);
  globalPo i2 = C_GLOB(o2);

  return globalCmp(i1, i2);
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

static void markGlobal(globalPo glb, gcSupportPo G) {
  if (glb->content != Null)
    markPtr(G, (ptrPo) &glb->content);
}

void markGlobals(gcSupportPo G) {
  for (int32 ix = 0; ix < numGlbVars; ix++)
    markGlobal(&glbVars[ix], G);
}

retCode glbDisp(ioPo out, termPo t, long depth, logical alt) {
  globalPo glb = C_GLOB(t);

  if (getGlobal(glb) != Null)
    return outMsg(out, "global: %s=%t", glb->name, glb->content);
  else
    return outMsg(out, "global: %s (undef)", glb->name);
}

termPo getGlobal(globalPo v) {
  return v->content;
}

char *globalVarName(globalPo v) {
  return v->name;
}

globalPo getGlobalVar(int32 varNo) {
  return &glbVars[varNo];
}

int32 globalVarNo(const char *nm) {
  return (int32) (globalVar(nm)->varNo);
}

logical glbIsSet(globalPo glb) {
  return (logical) (glb->content != Null);
}

termPo setGlobalVar(globalPo v, termPo e) {
  termPo prev = v->content;
  v->content = e;
  return prev;
}
