/*
  Output formatting functions for I/O library
  (c) 1994-2010 F.G. McCabe

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.
  
  Contact: fmccabe@gmail.com
*/

#include "config.h"		/* Invoke configuration header */
#include "io.h"
#include "formioP.h"

#include <stdarg.h>
#include <float.h>		/* For fp conversion */
#include <limits.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

static char *Int2StrByBase(char *str,uinteger i,unsigned int base);
static retCode outString(ioPo f,char *str,int len,int width,int precision,
		    char pad,logical leftPad);

retCode outInt(ioPo f,integer i)
{
  char buff[64];
  int pos = 0;

  if(i<0){
    buff[pos++]='-';
    i = -i;
  }

  Int2StrByBase(&buff[pos],(uinteger)i,10);

  return outStr(f,buff);
}

static inline char hxDgit(unsigned long h)
{
  if(h<10)
    return h|'0';
  else
    return h+'a'-10;
}

static char *Int2StrByBase(char *str,uinteger i,unsigned int base)
{
  if(i<base)
    *str++=hxDgit(i);
  else{
    str=Int2StrByBase(str,i/base,base);
    *str++=hxDgit(i%base);
  }
  *str='\0';
  return str;
}

retCode int2Str(integer i,char *buff,long len)
{
  if(i<0){
    *buff='-';
    if(Int2StrByBase(buff+1,-i,10)-buff>=0)
      return Ok;
    else
      return Error;
  }
  else if(Int2StrByBase(buff,i,10)-buff>=0)
    return Ok;
  else
    return Error;
}

static uniChar *int2UniByBase(integer i,int base,uniChar *buff,long len)
{
  if(len<2)
    return NULL;
  else if(i<base)
    *buff++=hxDgit(i);
  else{
    buff = int2UniByBase(i/base,base,buff,len-1);
    if(buff!=NULL)
      *buff++=hxDgit(i%base);
    else
      return NULL;
  }
  *buff='\0';
  return buff;
}

retCode int2Uni(integer i,int base,logical sign,uniChar *buff,long len)
{
  if(i<0){
    *buff++='-';
    if(int2UniByBase(-i,base,buff,len-1)!=NULL)
      return Ok;
    else
      return Error;
  }
  else{
    if(sign){
      *buff++='+';
      len--;
    }
    if(int2UniByBase(i,base,buff,len)!=NULL)
      return Ok;
    else
      return Error;
  }
}

retCode outInteger(ioPo f,integer i,int base,int width,int precision,
		     uniChar pad,logical left,char *prefix,
		     logical sign)
{
  char iBuff[128];
  int len;
  retCode ret;

  if(i>=0 && sign)
    prefix="+";
  else if(i<0){
    prefix="-";
    i = -i;
  }

  len = Int2StrByBase(iBuff,i,base)-iBuff;
  
  ret = outStr(f,prefix);
  
  if(ret==Ok)
    ret = outString(f,iBuff,len,width-strlen(prefix),precision,pad,left);

  return ret;
}


static retCode outOctal(ioPo f,long i,int width,int precision,uniChar pad,
			       logical left,char *prefix,logical sign,logical alt)
{
  char iBuff[64];
  int len = Int2StrByBase(iBuff,i,8)-iBuff;
  retCode ret;

  if(i>=0 && sign)
    prefix="+";

  if(!left)
    pad=' ';			/* We dont put trailing zeroes */

  ret = outStr(f,prefix);
  
  if(ret==Ok)
    ret = outString(f,iBuff,len,width-strlen(prefix),precision,pad,left);

  return ret;
}

static retCode outHex(ioPo f,long i,int width,int precision,uniChar pad,
			logical left,char *prefix,logical sign,logical alt)
{
  char iBuff[64];
  int len = Int2StrByBase(iBuff,i,16)-iBuff;
  retCode ret;

  if(alt)
    prefix="0x";

  if(!left)
    pad=' ';			/* We dont put trailing zeroes */

  ret = outStr(f,prefix);
  
  if(ret==Ok)
    ret = outString(f,iBuff,len,width-strlen(prefix),precision,pad,left);

  return ret;
}

/* Convert a floating point to decimal format */

static const double bit_values[] = {
  1.0E1L, 1.0E2L, 1.0E4L, 1.0E8L, 1.0E16L, 1.0E32L, 1.0E64L, 1.0E128L, 1.0E256L, 1.0E512L
};

#define max_bits 1023
  
static int number2Str(double x,int precision,char *dec,int *exp)
{
  int exp2;
  long exp10;
  int len=0;
  char *digits = dec;
	
/*
 *	We first deal with the special cases of zeroes, infinities and NaNs
 */

  *exp = 0;
	
  if(x==0.0L)
    return strlen(strcpy(digits,"0"));
  else if(x>DBL_MAX)
    return strlen(strcpy(digits,"Infinity"));
  else{
    frexp(x, &exp2);		/* Get the scale of the number */
	
    exp10 = exp2*log10(FLT_RADIX); /* Convert exponent to base10 exponent  */
	
    {				/* Fast way of scaling number */
      const double *p = bit_values;
      int n = exp10;
		
      if(n<0){			/* We have a number smaller than 10 */
	for (n = -n; n; p++, n >>= 1)
	  if (n & 1)
	    x *= *p;
      }
      else if(n>0){
	double f = 1.0;
			
	for (; n; p++, n >>= 1)
	  if(n & 1)
	    f *= *p;
	x /= f;
      }
    }
	
    while(x>=1.0L){
      x *= 0.1L;
      ++exp10;
    }
	
    while(x<0.1L){
      x *= 10.0L;
      --exp10;
    }

    while(precision-->=0){	/* Go to one more than required precision */
      double front;

      x = modf(x*10.0L,&front);	/* extract left-most digit */

      *digits++=hxDgit((int)front);
      len++;
    }

    *digits='\0';		/* Terminate digit string */

    *exp = exp10;
    return len-1;		/* we generated an extra digit */
  }
}

retCode outDouble(ioPo out,double x,char mode,int width,int precision,
		    uniChar pad,logical left,char *prefix,logical sign)
{
  char dec[DBL_DIG*2];		/* buffer for the decimal mantissae */
  char *d = dec;
  char buff[1024];		/* buffer to represent the number string */
  char *p = buff;
  char *eP = &buff[NumberOf(buff)-1]; /* end marker */
  retCode ret = Ok;

  int exp,len,sig;

  if(x<0){
    prefix="-";
    x = -x;
  }
  else if(sign)
    prefix="+";			/* Is the number signed? */
  
  len = sig = number2Str(x,DBL_DIG+1,dec,&exp);

  while(sig>0 && dec[sig-1]=='0')
    sig--;			/* chop off trailing zeroes */

  if(strcmp(dec,"Infinity")==0){
    ret = outStr(out,prefix);
    
    if(ret==Ok)
      ret = outString(out,dec,strlen(dec),width,precision,pad,left);
    return ret;
  }
  else{
    if(tolower(mode)=='e'||(tolower(mode)=='g' && 
			    (exp<-3||(precision==0?exp>DBL_DIG:exp>sig+1)))){
      *p++=*d++;			/* use scientific format */
      len--; sig--;
      *p++='.';
      if(precision>0){
	while(precision-->0)
	  if(len-->0)
	    *p++=*d++;
	  else
	    *p++=*d;		/* trailing zero */
      }
      else if(precision==0 && len>=0){
	if(sig>0){
	  while(sig-->0)
	    *p++=*d++;
	}
	else
	  *p++='0';
      }
      else			       /* ensure that we have the .0 trailing */
	*p++='0';

      *p++='E';			/* Show exponent sign */
      if(--exp<0){
	*p++='-';
	exp = -exp;
      }
      Int2StrByBase(p,exp,10);/* Show exponent value -- adjusted for leading digit*/
    }
    else if(exp<=0){		/* Use fixed point format */
      int prec = precision;

      *p++='0';
      *p++='.';

      if(precision==0)
	while(p<eP && exp++<0)
	  *p++='0';
      else
	while(precision>0 && p<eP && exp<0){
	  *p++='0';
	  precision--;
	  exp++;
	}

      if(prec!=0){
	while(p<eP && precision>0){
	  if(len-->0)
	    *p++=*d++;
	  else
	    *p++='0';
	  precision--;
	}
      }
      else{			/* display all available digits */
	if(sig>0){
	  while(p<eP && sig-->0)
	    *p++=*d++;
	}
	else
	  *p++='0';			/* 0.0 */
      }
      *p='\0';
    }
    else{
      while(p<eP && exp-->0)
	if(len-->0){
	  *p++=*d++;
	  sig--;
	}
	else
	  *p++=*d;

      if(p<eP && precision>0){
	*p++='.';
	while(p<eP && precision>0){	/* copy out the fractional part */
	  if(len-->0)
	    *p++=*d++;
	  else
	    *p++='0';
	  precision--;
	}
      }
      else if(p<eP && precision==0){
	*p++='.';
	if(sig>0){
	  while(p<eP && sig-->0)	/* copy out the fractional part */
	    *p++=*d++;
	}
	else{				/* ensure that we have the .0 trailing */
	  *p++='0';
	}
      }
      *p='\0';
    }

    ret = outStr(out,prefix);
    if(ret==Ok)
      ret = outString(out,buff,strlen(buff),width,strlen(buff),pad,left);
    return ret;
  }
}

retCode outFloat(ioPo out,double x)
{
  return outDouble(out,x,'g',0,0,' ',True,"",False);
}


retCode outUStr(ioPo f,uniChar *str)
{
  return outText(f,str,uniStrLen(str));
}

retCode outString(ioPo f,char *str,int len,int width,int precision,
		  char pad,logical leftPad)
{
  int gaps;
  retCode ret = Ok;

  lock(O_OBJECT(f));

  if(precision>0 && precision<len)
    len = precision;		/* we only show part of the string */

  if(width>0){			/* fixed width */
    if(len>width)
      len=width;		/* never print more than available width */

    if(!leftPad){		/* right justified */
      gaps = width-len;

      ret = outCText(f,str,len);

      while(ret==Ok && gaps-->0)
	ret = outChar(f,pad);
    }
    else{
      gaps = width-len;

      while(ret==Ok && gaps-->0)
	ret = outChar(f,pad);

      if(ret==Ok)
	ret = outCText(f,str,len);
    }
  }
  else
    ret = outCText(f,str,len);

  unlock(O_OBJECT(f));
  return ret;
}

static retCode quoteChar(ioPo f,uniChar ch,int *gaps)
{
  retCode ret;
  switch(ch){
  case '\a':
    ret = outStr(f,"\\a");
    (*gaps)--;               // An additional character
    break;
  case '\b':
    ret = outStr(f,"\\b");
    (*gaps)--;
    break;
  case '\x7f':
    ret = outStr(f,"\\d");
    (*gaps)--;
    break;
  case '\x1b':
    ret = outStr(f,"\\e");
    (*gaps)--;
    break;
  case '\f':
    ret = outStr(f,"\\f");
    (*gaps)--;
    break;
  case '\n': 
    ret = outStr(f,"\\n");
    (*gaps)--;
    break;
  case '\r': 
    ret = outStr(f,"\\r");
    (*gaps)--;
    break;
  case '\t': 
    ret = outStr(f,"\\t");
    (*gaps)--;
    break;
  case '\v': 
    ret = outStr(f,"\\v");
    break;
  case '\\':
    ret = outStr(f,"\\\\");
    (*gaps)--;
    break;
  case '\"':
    ret = outStr(f,"\\\"");
    (*gaps)--;
    break;
  default:
    if(ch<' '){
      ret = outChar(f,'\\');
      if(ret==Ok)
	ret = outChar(f,((ch>>6)&3)|'0');
      if(ret==Ok)
	ret = outChar(f,((ch>>3)&7)|'0');
      if(ret==Ok)
	ret = outChar(f,(ch&7)|'0');
      (*gaps)-=4;
    }
    else if(ch>255){
      ret = outStr(f,"\\+");
      if(ret==Ok)
	ret = outChar(f,hxDgit((ch>>12)&0xf));
      if(ret==Ok)
	ret = outChar(f,hxDgit((ch>>8)&0xf));
      if(ret==Ok)
	ret = outChar(f,hxDgit((ch>>4)&0xf));
      if(ret==Ok)
	ret = outChar(f,hxDgit(ch&0xf));
      if(ret==Ok)
	ret = outChar(f,';');
      (*gaps)-=6;
    }
    else
      ret = outChar(f,ch);
  }
  return ret;
}

static retCode dumpText(ioPo f,uniChar *str,int len)
{
  int gaps = 0;
  retCode ret = Ok;
  int ix;
  for(ix=0;ret==Ok && ix<len;ix++)
    ret = quoteChar(f,str[ix],&gaps);
  return ret;
}

retCode outUniString(ioPo f,uniChar *str,int len,int width,int precision,
		     uniChar pad,logical leftPad,logical alt)
{
  int gaps;
  retCode ret = Ok;

  lock(O_OBJECT(f));

  if(precision>0 && precision<len)
    len = precision;		/* we only show part of the string */

  if(width>0){			/* fixed width */
    if(len>width)
      len=width;		/* never print more than available width */

    if(!leftPad){		/* right justified */
      gaps = width-len;
      
      if(alt){
        while(ret == Ok && len-->0){
          uniChar ch = *str++;
	  quoteChar(f,ch,&gaps);
        }
      }
      else
        ret = outText(f,str,len);

      while(ret==Ok && gaps-->0)
	ret = outChar(f,pad);
    }
    else{
      gaps = width-len;

      while(ret==Ok && gaps-->0)
	ret = outChar(f,pad);
      if(ret==Ok)
        ret = outText(f,str,len);
    }
  }
  else if(alt)
    ret = dumpText(f,str,len);
  else
    ret = outText(f,str,len);	/* variable width */
  unlock(O_OBJECT(f));

  return ret;
}

/**********************************************************************/
/*               Display a formatted message                          */
/**********************************************************************/

static fileMsgProc procs[256];		/* What to invoke... */

static void initMsgProcs(void)
{
  static logical inited = False;

  if(!inited){
    int i;
    for(i=0;i<255;i++)
      procs[i]=NULL;
    inited = True;
  }
}

void installMsgProc(char key,fileMsgProc proc)
{
  initMsgProcs();
  procs[(unsigned int)key] = proc;
}

/* We have our own version of fprintf too */

/* This one is used in april-log_msg */  
retCode __voutMsg(ioPo f,unsigned char *fmt,va_list args)
{
  retCode ret = Ok;
  
  while(ret==Ok && *fmt!='\0'){
    switch(*fmt){
    case '%':{
      unsigned char c;
      long width=0;		/* Maximum width of field */
      long precision=0;		/* Minimum width or precision of field */
      long depth=LONG_MAX;      /* Maximum depth of structure */
      char pad=' ';
      char *prefix="";
      logical sign=False;
      logical alternate=False;
      logical leftPad = True;
      logical overridePrecision = False;
      logical overrideDepth = False;
      logical longValue=False;

      fmt++;

      while(strchr("0 -#+l",*fmt)!=NULL){
	switch(*fmt++){
	case '0':
	  pad = '0';
	  continue;
	case ' ':
	  prefix=" ";
	  continue;
	case '+':
	  sign=True;
	  continue;
	case 'l':
	  longValue=True;
	  continue;
	case '#':
	  alternate=True;
	  continue;
	case '-':
	  leftPad=False;
	  continue;
	}
      }

      while(isNdChar(c=*fmt++))	/* extract the width field */
	width = width*10+(c&0xf);
        
      while(strchr(".,",(char)c)!=NULL){
        if(c=='.'){		/* We have a precision ... */
	  overridePrecision=True;
	  while(isNdChar(c=*fmt++))
	    precision = precision*10+(c&0xf);
        }
        else if(c==','){
          overrideDepth=True;
          depth=0;
	  while(isNdChar(c=*fmt++))
	    depth = depth*10+(c&0xf);
        }
        else
          break;
      }
          
      if(procs[c]!=NULL){
	void *data = (void *)va_arg(args, void*); /* pick up a special value */
	ret = procs[(unsigned int)c](f,data,depth,precision,alternate);
      }
      else
	switch(c){
        case '_':
          ret = flushFile(f);
          break;
	case 'c':{		/* Display an integer value as a char */
	  integer i = (integer)(longValue?va_arg(args,integer):va_arg(args,int)); 
	  
	  ret = outChar(f,i);
	  break;
	}
	case 'd':{		/* Display an integer value */
	  integer i = (integer)(longValue?va_arg(args,integer):va_arg(args,int)); 
	  
	  ret = outInteger(f,i,10,width,precision,pad,leftPad,prefix,sign);
	  break;
	}
	case 'u':{		/* Display a number as unsigned */
	  uinteger i = (uinteger)(longValue?va_arg(args,uinteger):va_arg(args,unsigned int)); 
	  char iBuff[64];
	  int len;

	  if(!leftPad)
	    pad=' ';		/* We dont put trailing zeroes */

	  len = Int2StrByBase(iBuff,i,10)-iBuff;

          ret = outStr(f,prefix);
          if(ret==Ok)
	    ret = outString(f,iBuff,len,width,precision,pad,leftPad);
	  break;
	}
	case 'o':{		/* Display an octal value */
	  integer i = (integer)(longValue?va_arg(args,integer):va_arg(args,long)); 

	  ret = outOctal(f,i,width,precision,pad,leftPad,prefix,sign,alternate);
	  break;
	}
	case 'x':{		/* Display a hex value */
	  integer i = (integer)(longValue?va_arg(args,integer):va_arg(args,long)); 

	  ret = outHex(f,i,width,precision,pad,leftPad,prefix,sign,alternate);
	  break;
	}
	case 'b':{		/* Display a binary value */
	  integer i=(integer)(longValue?va_arg(args,integer):va_arg(args,long)); 
	  
	  ret = outInteger(f,i,2,width,precision,pad,leftPad,prefix,sign);
	  break;
	}
	case 'g':
	case 'G':
	case 'e':
	case 'E':	
	case 'F':
	case 'f':{		/* Display floating point number */
	  double num = (double)va_arg(args,double);

	  if(!overridePrecision) /* default precision for floats */
	    precision=6;
	  ret = outDouble(f,num,c,width,precision,pad,leftPad,prefix,sign);
	  break;
	}
	case 's':{		/* Display a string */
	  char *str = (char*)va_arg(args,char *);
	  
	  if(str!=NULL)
	    ret = outString(f,str,strlen(str),width,precision,' ',leftPad);
	  else
	    ret = outStr(f,"(NULL)");
	  break;
	}

	case 'S':{		/* Display a data block */
	  long len = (long)va_arg(args,long);
	  char *str = (char*)va_arg(args,char *);
	  
	  if(str!=NULL){
	    int i;

	    for(i=0;ret==Ok && i<len;i++)
	      if(isprint((unsigned char)str[i]))
		ret = outChar(f,str[i]);
	      else
		ret = outMsg(f,"\\%x\\",str[i]&0xff);
	  }
	  else
	    ret = outStr(f,"(NULL)");
	  break;
	}

	case 'U':{		/* Display a uniCode string */
	  uniChar *str = (uniChar*)va_arg(args,uniChar *);
	  
	  if(str!=NULL){
	    ret = outStr(f,prefix);
	    if(ret==Ok)
	      ret = outUniString(f,str,uniStrLen(str),width,precision,' ',leftPad,alternate);
	  }
	  else
	    ret = outStr(f,"(NULL)");
	  break;
	}

	case 'Z':{		/* Display a chunk of a URL */
	  uniChar *str = (uniChar*)va_arg(args,uniChar *);
	  
	  if(str!=NULL){
	    ret = outStr(f,prefix);
	    if(ret==Ok){
              long len = uniStrLen(str);
              
              if(width>0 && len>width)
                len = width;

              while(ret == Ok && len-->0){
                uniChar ch = *str++;
        
                switch(ch){
                  default:
                    if(ch>32 && ch<127){
                    ret = outChar(f,ch&0xff);
                    continue;
                  }               // Else fall through
         
                  case '!': case '"': case '#': case '$': case '%': case '&':
                  case '\'': case '(': case ')': case '+': case ',': case '/':
                  case ':': case ';': case '<': case '=': case '>': case '?':
                  case '@': case '[': case '\\': case ']': case '^': case '`':
                  case '{': case '|': case '}': case '~':
                    ret = outChar(f,'%');
                    if(ret==Ok && len-->0)
                      ret = outChar(f,hxDgit((ch>>4)&0xf));
                    if(ret==Ok && len-->0)
                      ret = outChar(f,hxDgit(ch&0xf));
                    continue;
                }
              }
            }
	  }
	  else
	    ret = outStr(f,"(NULL)");
	  break;
	}

	default:
	  ret = outChar(f,c);
	}
      break;
    }
    
    default:
      ret = outChar(f,*fmt++);
    }
  }
  return ret;
}

retCode outMsg(ioPo f,char *fmt,...)
{
  if(f!=NULL){
    retCode ret;

    lock(O_OBJECT(f));
    
    va_list args;		/* access the generic arguments */
    va_start(args,fmt);		/* start the variable argument sequence */

    ret = __voutMsg(f,(unsigned char*)fmt,args);

    va_end(args);

    unlock(O_OBJECT(f));

    return ret;
  }
  else
    return Error;
}

retCode logMsg(ioPo out,char *fmt,...)
{
  retCode ret = Ok;
  
  if(out!=NULL){
    time_t now;
    va_list ap;

    lock(O_OBJECT(out));
    
    va_start(ap,fmt);

    if(time(&now)!=-1){
      struct tm *t = localtime(&now);
      char stamp[256];
      strftime(stamp,256,"%a %e/%b/%Y %X",t);

      ret = outMsg(out,"%s - ",stamp);
      if(ret==Ok)
        ret = __voutMsg(out,(unsigned char*)fmt,ap);
      if(ret==Ok)
        ret = outMsg(out,"\n");
    }
    else{
      ret = __voutMsg(out,(unsigned char*)fmt,ap);
      if(ret==Ok)
        ret = outMsg(out,"\n");
    }
      
    va_end(ap);

    unlock(O_OBJECT(out));
  }
  flushFile(out);
  return ret;
}

