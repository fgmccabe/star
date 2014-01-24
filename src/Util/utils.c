/*
 * utilities for use by the compiler
 */
#include "config.h"
#include <ooio.h>
#include <unicode.h>

#include "utils.h"

static long counter = 0;

uniChar *genSym(char *prefix)
{
  uniChar nameU[1024];

  strMsg(nameU,NumberOf(nameU),"%s%ld",prefix,counter++);

  return uniIntern(nameU);
}

uniChar *genUSym(uniChar *prefix)
{
  uniChar nameU[1024];

  strMsg(nameU,NumberOf(nameU),"%U%ld",prefix,counter++);

  return uniIntern(nameU);
}

string mkLabel(uniChar *prefix,int variant)
{
  uniChar nameU[1024];

  strMsg(nameU,NumberOf(nameU),"%U%ld",prefix,variant);

  return uniIntern(nameU);
}

string mkIdent(char *prefix,int variant)
{
  uniChar nameU[1024];

  strMsg(nameU,NumberOf(nameU),"%s%ld",prefix,variant);

  return uniIntern(nameU);
}

string mkInterned(char *symbol)
{
  uniChar nameU[1024];

  strMsg(nameU,NumberOf(nameU),"%s",symbol);

  return uniIntern(nameU);
}  




