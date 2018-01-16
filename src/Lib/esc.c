#include "config.h"
#include <ooio.h>
#include "engine.h"
#include "escape.h"
#include "signature.h"

static hashPo escapes = NULL;
static poolPo escPool;

#undef escape
#define escape(Fun,Visible,Sys,Sig,Cmnt)\
extern retCode g_##Fun(processPo p,ptrPo tos);\
  installEscape(#Fun,Sig,g_##Fun);

void initEscapes()
{
  if(escapes==NULL){
    escapes = NewHash(64,(hashFun)uniHash,(compFun)uniCmp,NULL);
    escPool = newPool(sizeof(EscapeRec), 128);

#include "escapes.h"
#undef escape

  }
}

void installEscape(char *name, char *sig, libFun fun)
{
  escapePo esc = (escapePo)allocPool(escPool);

  esc->name = uniDuplicate(name);
  esc->sig = uniDuplicate(sig);
  esc->fun = fun;
  integer arity;

  funSigArity(sig, &arity);
  esc->arity = arity;

  hashPut(escapes,uniDuplicate(name),esc);
}

escapePo findEscape(char *name)
{
  return (escapePo)hashGet(escapes,name);
}



