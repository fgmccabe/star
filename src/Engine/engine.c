/*
 * top-level run-time functions
 */
#include "config.h"
#include <ooio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "engine.h"
#include "heap.h"
#include "decode.h"
#include "opcodes.h"
#include "escapes.h"


static poolPo prPool;			/* pool of processes */
static uniChar UMAIN[] = {'_','m','a','i','n',0};

void initEngine()
{
  prPool = newPool(sizeof(ProcessRec),32);
  initEscapes();
}

int loadAndGo(uniChar *boot,int argc,char* args[])
{
  ioPo in = openURI(boot,rawEncoding);

  if(in!=Null){
    uniChar ch = inCh(in);		/* We skip over #! */

    if(ch=='#'){			/* look for standard #!/.... header */
      if((ch=inCh(in))=='!'){
	while((ch=inCh(in))!=uniEOF && ch!='\n')
	  ;			        /* consume the interpreter statement */
      }
      else{
	unGetChar(in,ch);
	unGetChar(in,'#');
      }
    }
    else
      unGetChar(in,ch);

    hashPo pkg = decodePkg(in);
    if(pkg!=Null){
      methodPo umain = (methodPo)hashGet(pkg,UMAIN);
      if(umain!=Null){
	closurePo mainCl = allocate(currHeap,umain);
	assert(mainCl!=Null);
	processPo p = newProcess(mainCl);
	run(p,currHeap);
	return 0;
      }
      else{
	syserr("no _main");
	return 1;
      }
    }
    else{
      syserr("cannot load boot file");
      return 99;
    }
  }
  else{
    syserr("cannot open boot file");
    return 100;
  }
}

static uint16 haltCode[] = { Halt };

processPo newProcess(closurePo cl)
{
  processPo P = (processPo)allocPool(prPool);

  P->prog = cl;
  P->pc = entryPoint(cl);
  P->stackBase = (ptrPo)malloc(sizeof(uint64)*stackSize);
  P->stackLimit = &P->stackBase[stackSize];

  P->fp = (framePo)P->stackLimit;

  // cap the stack with a halting stop.

  ptrPo sp = (ptrPo)P->fp;
  *--sp = (uint64)cl;
  *--sp = (uint64)&haltCode[0];

  P->sp = sp;

  return P;
}
