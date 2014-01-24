/*
 * Simple peephole optimization
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "compile.h"
#include "abstract.h"
#include "assem.h"

static retCode peepSeg(void *n,void *r,void *c)
{
  segmentPo seg = (segmentPo)r;

  insPo i = seg->first;

  if(i!=Null){
    do{
      switch(i->op){
      case Jmp:{			/* Look for a jump to a jump */
	insPo tgt = i->o.oneLbl.lbl->ins;
	if(tgt!=Null){
	  do{
	    if(tgt->op==Jmp){
	      i->o.oneLbl.lbl = tgt->
	    }
	  } while(tgt->op==DefineLbl && tgt!=i->o.oneLbl.lbl->ins);
	}
      }
      }

      i = i->next;
    } while(first!=seg->first);
  }
  return Ok;

}
void peepHole(mtdPo mtd){
  
}
