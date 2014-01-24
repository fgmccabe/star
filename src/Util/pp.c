/*
 * Implementation of pretty print
 */
#include "config.h"
#include <ooio.h>
#include <unicode.h>

#include "pp.h"

PPDisplay initDisplay(ioPo f)
{
  PPDisplay disp = {f,0,Ok};
  return disp;
}

static retCode appendChar(ppDisplayPo disp,policyPo pol,char ch);

retCode ppAppend(ppDisplayPo disp,policyPo pol,char *str)
{
  while(disp->ret==Ok && *str!='\0')
    disp->ret = appendChar(disp,pol,*str++);
  return disp->ret;
}

retCode appendChar(ppDisplayPo disp,policyPo pol,char ch)
{
  if(disp->ret==Ok){
    disp->ret = outChar(disp->file,ch);
    if(ch=='\n'){
      disp->currColumn = 0;
      for(int ix=0;disp->ret==Ok && ix<pol->indent;ix++)
	disp->ret = appendChar(disp,pol,' ');
    }
    disp->currColumn++;
  }
  return disp->ret;
}

retCode ppAppendC(ppDisplayPo disp,policyPo pol,uniChar ch)
{
  if(disp->ret==Ok){
    disp->ret = outMsg(disp->file,"%#C",ch);
    disp->currColumn=outColumn(disp->file);
  }
  return disp->ret;
}

retCode ppAppendU(ppDisplayPo disp,policyPo pol,uniChar *str)
{
  while(disp->ret==Ok && *str!='\0')
    disp->ret = appendChar(disp,pol,*str++);
  return disp->ret;
}

static logical isCafeIdentifier(uniChar *str)
{
  if(*str!='\0'){
    uniChar ch = *str++;

    if(!(('a'<=ch && ch<='z')||
	 ('A'<=ch && ch<='Z')||
	 ch=='_' ||
	 ch=='.' ||
	 ch=='$' ||
	 ch=='@' ||
	 ch=='#'))
      return False;

    while(*str!='\0'){
      ch = *str++;
      if(!(('a'<=ch && ch<='z')||
	   ('A'<=ch && ch<='Z')||
	   ('0'<=ch && ch<='9')||
	   ch=='_' ||
	   ch=='.' ||
	   ch=='$' ||
	   ch=='@' ||
	   ch=='#'))
	return False;
    }
    return True;
  }
  else
    return False;
}

retCode ppAppendId(ppDisplayPo disp,policyPo pol,uniChar *str)
{
  if(disp->ret==Ok){
    if(isCafeIdentifier(str)){
      disp->ret = outMsg(disp->file,"%U",str);
      disp->currColumn=outColumn(disp->file);
    }
    else{
      disp->ret = outMsg(disp->file,"\'%U\'",str);
      disp->currColumn=outColumn(disp->file);
    }
  }  
  return disp->ret;
}

retCode ppAppendQ(ppDisplayPo disp,policyPo pol,uniChar *str)
{
  if(disp->ret==Ok){
    disp->ret = outMsg(disp->file,"\"%#U\"",str);
    disp->currColumn=outColumn(disp->file);
  }  
  return disp->ret;
}

retCode ppAppendI(ppDisplayPo disp,policyPo pol,integer i)
{
  uniChar buffer[1024];

  strMsg(buffer,NumberOf(buffer),"%ld",i);
  return ppAppendU(disp,pol,buffer);
}

retCode ppAppendF(ppDisplayPo disp,policyPo pol,double d)
{
  uniChar buffer[1024];

  strMsg(buffer,NumberOf(buffer),"%f",d);
  return ppAppendU(disp,pol,buffer);
}

