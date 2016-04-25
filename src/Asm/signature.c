
#include <ooio.h>
#include "signature.h"

uniChar integerSig[] = { INTEGER_SIG, 0};
uniChar stringSig[] = { STRING_SIG, 0};
uniChar floatSig[] = { FLOAT_SIG, 0};

static logical validSig(uniChar *sig,int32 *start,int32 end);

logical validSignature(uniChar *sig)
{
  int32 pos = 0;
  int32 end = uniStrLen(sig);

  return validSig(sig,&pos,end) && pos==end;
}

static logical skipId(uniChar *sig,int32 *start,int32 end)
{
  while(*start<end)
    if(sig[(*start)++]==';')
      break;
  return *start<=end;
}

logical validSig(uniChar *sig,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case usrSig:
  case tVrSig:
    return skipId(sig,start,end);
  case rawSig:{
    while(*start<end){
      uniChar ch = (*start)++;
      if(sig[ch]==';')
        break;
      else if(!isNdChar(ch))
        return False;
    }
    return *start<=end;
  }
  case tplSig:{
    while(*start<end && sig[*start]!=')')
      if(!validSig(sig,start,end))
      	return False;
    if(sig[*start]==')'){
      (*start)++;
      return True;
    }
    else
      return False;
  }
  case fceSig:{
    while(*start<end && sig[*start]!='}')
      if(!skipId(sig,start,end) || !validSig(sig,start,end))
        return False;
    if(sig[*start]=='}'){
      (*start)++;
      return True;
    }
    else
      return False;
  }
  case funSig:				/* Function signature */
  case conSig:				/* Type constructor */
  case xstSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    return validSig(sig,start,end) &&
      validSig(sig,start,end);
  case repSig:
    return validSig(sig,start,end);
  default:
    return False;			/* Not a valid signature */
  }
  return True;
}

static retCode tplArity(uniChar *sig,int32 *arity,int32 *start,int32 end);

static retCode funArity(uniChar *sig,int32 *arity,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case usrSig:
  case conSig:				/* Type constructor */
  case tVrSig:
    return Error;			/* top-level must be a tuple type */
  case tplSig:{
    *arity = 0;
    while(*start<end && sig[*start]!=')'){
      tryRet(skipSig(sig,start,end));
      (*arity)++;
    }

    if(sig[*start]==')'){
      (*start)++;
      return Ok;
    }
    else
      return Error;
  }
  case funSig:				/* Function signature */
    return tplArity(sig,arity,start,end);
  case xstSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    tryRet(skipSig(sig,start,end));
    return funArity(sig,arity,start,end);
  default:
    return Error;			/* Not a valid signature */
  }
  return Error;
}

retCode functionArity(uniChar *sig,int32 *arity)
{
  int32 pos = 0;
  int32 end = uniStrLen(sig);

  return funArity(sig,arity,&pos,end);
}

static retCode tplArity(uniChar *sig,int32 *arity,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case tplSig:{
    *arity = 0;
    while(*start<end && sig[*start]!=')'){
      tryRet(skipSig(sig,start,end));
      (*arity)++;
    }

    if(sig[*start]==')'){
      (*start)++;
      return Ok;
    }
    else
      return Error;
  }
  case xstSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    tryRet(skipSig(sig,start,end));
    return tplArity(sig,arity,start,end);
  default:
    return Error;			/* Not a valid signature */
  }
}

retCode tupleArity(uniChar *sig,int32 *arity)
{
  int32 pos = 0;
  int32 end = uniStrLen(sig);

  return tplArity(sig,arity,&pos,end);
}

retCode skipSig(uniChar *sig,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case usrSig:
  case tVrSig:
  case rawSig:
    while(*start<end)
      if(sig[(*start)++]==';')
      	break;
    if(*start<end)
      return Ok;
    else
      return Error;
  case tplSig:{
    while(*start<end && sig[*start]!=')')
      tryRet(skipSig(sig,start,end));

    if(sig[*start]==')'){
      (*start)++;
      return Ok;
    }
    else
      return Error;
  }
  case fceSig:{
    while(*start<end && sig[*start]!='}'){
      if(!skipId(sig,start,end))
        return Error;
      else
        tryRet(skipSig(sig,start,end));
    }

    if(sig[*start]=='}'){
      (*start)++;
      return Ok;
    }
    else
      return Error;
  }
  case funSig:				/* Function signature */
  case conSig:				/* Type constructor */
  case xstSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    tryRet(skipSig(sig,start,end));
    return skipSig(sig,start,end);
  case repSig:
    return skipSig(sig,start,end);
  default:
    return Error;			/* Not a valid signature */
  }
}

retCode showSig(ioPo out,uniChar *sig)
{
  int32 pos = 0;
  int32 end = uniStrLen(sig);

  return showSignature(out,sig,&pos,end);
}

static retCode showSigId(ioPo out,uniChar *sig,int32 *start,int32 end){
  while(*start<end){
    uniChar ch = (*start)++;
    if(sig[ch]==';')
      break;
    else
      tryRet(outChar(out,ch));
  }
  if(*start<end)
    return Ok;
  else
    return Error;
}

retCode showSignature(ioPo out,uniChar *sig,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case tVrSig:
    tryRet(outChar(out,'%'));
  case rawSig:
  case usrSig:
    return showSigId(out,sig,start,end);
  case tplSig:{
    tryRet(outChar(out,'('));
    char *sep = "";

    while(*start<end && sig[*start]!=')'){
      tryRet(outStr(out,sep));
      sep = ", ";
      tryRet(showSignature(out,sig,start,end));
    }

    if(sig[*start]==')'){
      (*start)++;
      return outChar(out,')');
    }
    else
      return Error;
  }
  case fceSig:{
    tryRet(outChar(out,'{'));
    char *sep = "";

    while(*start<end && sig[*start]!='}'){
      tryRet(outStr(out,sep));
      sep = ", ";
      tryRet(showSigId(out,sig,start,end));
      tryRet(outStr(out,":"));
      tryRet(showSignature(out,sig,start,end));
    }

    if(sig[*start]=='}'){
      (*start)++;
      return outChar(out,'}');
    }
    else
      return Error;
  }
  case funSig:				/* Function signature */
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out,"=>"));
    return showSignature(out,sig,start,end);
  case conSig:				/* Type constructor */
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," <=> "));
    return showSignature(out,sig,start,end);
  case xstSig:				/* Existential quantifier */
    tryRet(outStr(out,"exists "));
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," such that "));
    return showSignature(out,sig,start,end);
  case allSig:				/* Universal quantifier */
    tryRet(outStr(out,"all "));
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," such that "));
    return showSignature(out,sig,start,end);
  case repSig:
    tryRet(outStr(out,"["));
    tryRet(showSignature(out,sig,start,end));
    return outStr(out,"]");
  default:
    return Error;			/* Not a valid signature */
  }
}

