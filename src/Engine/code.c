//
// Created by Francis McCabe on 1/15/18.
//

#include "codeP.h"
#include "termP.h"
#include "formioP.h"

static hashPo labels;
static poolPo labelPool;

static integer labelHash(labelPo lbl);
static comparison labelCmp(labelPo lb1, labelPo lb2);
static retCode labelDel(labelPo lbl, labelPo mtd);

void initCode() {
  labels = NewHash(1024, (hashFun) labelHash, (compFun) labelCmp, (destFun) labelDel);
  labelPool = newPool(sizeof(Label),1024);
}

labelPo declareLbl(char *name, integer arity) {
  Label tst = {.name=name,.arity=arity};
  labelPo lbl = hashGet(labels,&tst);

  if(lbl==Null){
    lbl = (labelPo)allocPool(labelPool);
    lbl->arity = arity;
    lbl->name = uniDuplicate(name);
    lbl->mtd = Null;
    lbl->clss.sig = (termPo)labelClass;
    hashPut(labels,lbl,lbl);
  }
  return lbl;
}

integer labelHash(labelPo lbl) {
  return uniHash(lbl->name) * 37 + lbl->arity;
}

comparison labelCmp(labelPo lb1, labelPo lb2) {
  comparison comp = uniCmp(lb1->name, lb2->name);

  if (comp == same) {
    if (lb1->arity < lb2->arity)
      comp = smaller;
    else if (lb1->arity > lb2->arity)
      comp = bigger;
  }
  return comp;
}

retCode labelDel(labelPo lbl, labelPo mtd){
  uniDestroy(mtd->name);
  freePool(labelPool,mtd);
  return Ok;
}

static void markMtd(heapPo h,methodPo mtd){
  
}

static retCode markLabel(void *n,void *r,void *c){
  labelPo lbl = (labelPo)r;
  heapPo h = (heapPo)c;

  if(lbl->mtd!=Null)
    markMtd(h,lbl->mtd);
  return Ok;
}

void markLabels(heapPo heap){
  ProcessTable(markLabel,labels,heap);
}



retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  methodPo mtd = (methodPo) data;
  constantPo mtdCon = codeLiterals(mtd);
  labelPo lbl = C_LBL(mtdCon->data);

  return outMsg(f, "%Q/%d", &lbl->name, lbl->arity);
}


