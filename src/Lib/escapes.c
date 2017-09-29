#include "config.h"
#include <ooio.h>

#include "escape.h"
#include "libNames.h"

static hashPo escapes = NULL;

void initEscapes()
{
  if(escapes==NULL){
    escapes = NewHash(64,(hashFun)uniHash,(compFun)uniCmp,NULL);

    installSystem();
    installShow();
  }
}

void installEscape(char *name,escapePo escape)
{
  //hashPut(escapes,mkInterned(name),escape);
}

escapePo findEscape(char *name)
{
  return (escapePo)hashGet(escapes,name);
}


