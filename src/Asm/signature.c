
#include <ooio.h>
#include "signature.h"

uniChar integerSig[] = { rawInt, 0};
uniChar stringSig[] = { rawString, 0};
uniChar floatSig[] = { rawFloat, 0};

static logical validSig(uniChar *sig,int32 *start,int32 end);

logical validSignature(uniChar *sig)
{
  int32 pos = 0;
  int32 end = uniStrLen(sig);

  return validSig(sig,&pos,end) && pos==end;
}

logical validSig(uniChar *sig,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case intSig:
  case fltSig:
  case strSig:
    return True;
  case usrSig:
  case tVSig:
    while(*start<end)
      if(sig[(*start)++]==';')
	break;
    return *start<end;
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
  case funSig:				/* Function signature */
  case conSig:				/* Type constructor */
  case exSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    return validSig(sig,start,end) &&
      validSig(sig,start,end);
  default:
    return False;			/* Not a valid signature */
  }
  return True;
}

static retCode tplArity(uniChar *sig,int32 *arity,int32 *start,int32 end);

static retCode funArity(uniChar *sig,int32 *arity,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case intSig:
  case fltSig:
  case strSig:
  case usrSig:
  case conSig:				/* Type constructor */
  case tVSig:
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
  case escSig:				/* Escape signature */
  case funSig:				/* Function signature */
    return tplArity(sig,arity,start,end);
  case exSig:				/* Existential quantifier */
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
  case exSig:				/* Existential quantifier */
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
  case intSig:
  case fltSig:
  case strSig:
    return Ok;
  case usrSig:
  case tVSig:
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
  case escSig:				/* Escape signature */
  case funSig:				/* Function signature */
  case conSig:				/* Type constructor */
  case exSig:				/* Existential quantifier */
  case allSig:				/* Universal quantifier */
    tryRet(skipSig(sig,start,end));
    return skipSig(sig,start,end);

  default:
    return Error;			/* Not a valid signature */
  }
}

retCode showSignature(ioPo out,uniChar *sig,int32 *start,int32 end)
{
  switch(sig[(*start)++]){
  case intSig:
    return outMsg(out,"integer_");
  case fltSig:
    return outMsg(out,"float_");
  case strSig:
    return outMsg(out,"string_");
  case tVSig:
    tryRet(outChar(out,'%'));
  case usrSig:{
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
  case escSig:				/* Escape signature */
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out,"->"));
    return showSignature(out,sig,start,end);
  case funSig:				/* Function signature */
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out,"=>"));
    return showSignature(out,sig,start,end);
  case conSig:				/* Type constructor */
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," <=> "));
    return showSignature(out,sig,start,end);
  case exSig:				/* Existential quantifier */
    tryRet(outStr(out,"exists "));
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," such that "));
    return showSignature(out,sig,start,end);
  case allSig:				/* Universal quantifier */
    tryRet(outStr(out,"all "));
    tryRet(showSignature(out,sig,start,end));
    tryRet(outStr(out," such that "));
    return showSignature(out,sig,start,end);
  default:
    return Error;			/* Not a valid signature */
  }
}

