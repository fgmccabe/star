/*
  String file handling functions 
  (c) 1994-1998 Imperial College & F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

#include "config.h"		/* pick up standard configuration header */
#include "ioStringP.h"
#include "formioP.h"

#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

/* Set up the string file class */

static void initStringClass(classPo class,classPo req);
static void StringDestroy(objectPo o);
static void StringInit(objectPo list,va_list *args);

static retCode stringInChar(ioPo f,uniChar *ch);
static retCode stringInBytes(ioPo f,byte *ch,long count,long *actual);
static retCode stringOutChar(ioPo f,uniChar ch);
static retCode stringUngetChar(ioPo f,uniChar ch);
static retCode stringOutBytes(ioPo f,byte *b,long count,long *actual);
static retCode stringBackByte(ioPo f,byte b);

static retCode stringAtEof(ioPo io);
static retCode stringInReady(ioPo f);
static retCode stringOutReady(ioPo f);
static retCode stringFlusher(ioPo f,long count);
static retCode stringSeek(ioPo f,long count);
static retCode stringClose(ioPo f);

StringClassRec StringClass = {
  {
    (classPo)&IoClass,                      /* parent class is io object */
    "string",                               /* this is the string class */
    NULL,
    initStringClass,                        /* String class initializer */
    O_INHERIT_DEF,                          /* String object element creation */
    StringDestroy,                          /* String objectdestruction */
    O_INHERIT_DEF,                          /* erasure */
    StringInit,                             /* initialization of a string object */
    sizeof(StringObject),                   /* size of a string object */
    NULL,                                  /* pool of string values */
    PTHREAD_ONCE_INIT,			    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL
  },
  {
    stringInChar,                         /* inChar  */
    stringOutChar,                        /* outChar  */
    stringUngetChar,                      /* ungetChar  */
    stringInBytes,                        /* inByte  */
    stringOutBytes,                       /* outBytes  */
    stringBackByte,                       /* backByte */
    stringAtEof,			  /* at end of file? */
    stringInReady,                        /* readyIn  */
    stringOutReady,                       /* readyOut  */
    stringFlusher,                        /* flush  */
    stringSeek,                           /* seek  */
    stringClose                           /* close  */
  }
};

classPo stringClass = (classPo)&StringClass;

static void initStringClass(classPo class,classPo req)
{
}

static void setEncoding(stringPo f,ioEncoding encoding);

// IO initialization should already be done at this point
static void StringInit(objectPo o,va_list *args)
{
  stringPo f = O_STRING(o);

  // Set up the buffer pointers
  f->string.pos = 0;
  setEncoding(f,va_arg(*args,ioEncoding)); /* set up the encoding */
  f->string.buffer = va_arg(*args,uniChar *);
  f->string.len = va_arg(*args,long);   /* set up the buffer */
  f->io.mode = va_arg(*args,ioState);   /* set up the access mode */
  f->string.resizeable = va_arg(*args,logical); /* is this string resizeable? */
}

static void StringDestroy(objectPo o)
{
  stringPo str = O_STRING(o);
  if(str->string.resizeable)
    free(str->string.buffer);
}

// Implement class string functions
static retCode stringInChar(ioPo io,uniChar *ch)
{
  stringPo f = O_STRING(io);

  if(f->string.pos<f->string.len){
    *ch=f->string.buffer[f->string.pos++];
    return Ok;
  }
  else
    return Eof;
}

static retCode stringUngetChar(ioPo io,uniChar ch)
{
  stringPo f = O_STRING(io);

  if(f->string.pos>0){
    f->string.buffer[--f->string.pos]=ch;
    f->io.status = Ok;
    return Ok;
  }
  else
    return Error;
}

static retCode stringSeek(ioPo io,long count)
{
  stringPo f = O_STRING(io);

  if(count>=0 && count<f->string.pos){
    f->string.pos = count;
    return Ok;
  }
  else
    return Fail;
}

static retCode stringInBytes(ioPo io,byte *ch,long count,long *actual)
{
  retCode ret = Ok;
  long remaining = count;
  stringPo f = O_STRING(io);

  while(ret==Ok && remaining>0){
    if(f->string.pos>=f->string.len){
      if(remaining==count)
        ret = Eof;
      break;
    }
    else{
      *ch++=f->string.buffer[f->string.pos++];
      remaining--;
    }
  }
  *actual = count-remaining;

  return ret;
}

static retCode stringOutChar(ioPo io,uniChar ch)
{
  stringPo f = O_STRING(io);

  if(f->string.pos>=f->string.len){
    if(f->string.resizeable){
      long nlen = f->string.len+(f->string.len>>1); /* allow for some growth */
      uniChar *nbuff = realloc(f->string.buffer,sizeof(uniChar)*nlen);
      if(nbuff!=NULL){
        f->string.buffer = nbuff;
        f->string.len = nlen;
      }
      else
        return ioErrorMsg(io,"could not allocate more space for string");
    }
    else{
      return Ok;			/* Silently drop actual output */
    }
  }

  f->string.buffer[f->string.pos++]=ch;
  return Ok;
}

static retCode stringOutBytes(ioPo io,byte *b,long count,long *actual)
{
  retCode ret = Ok;
  long remaining = count;

  while(ret==Ok && remaining>0){
    ret = stringOutChar(io,(uniChar)*b++);
    remaining--;
  }
  *actual = count-remaining;
  return ret;
}

static retCode stringBackByte(ioPo io,byte b)
{
  stringPo f = O_STRING(io);

  if(f->string.pos>0){
    f->string.buffer[--f->string.pos]=b;
    return Ok;
  }
  else
    return Error;
}


static void setEncoding(stringPo f,ioEncoding encoding)
{
  f->string.encoding = encoding;

#if 0
  switch(encoding){
  case utf16Encoding:
    f->string.charOut = utf16OutChar;
    f->string.charIn = utf16InChar;
    break;
  case utf16EncodingSwap:
    f->string.charOut = utf16SwapOutChar;
    f->string.charIn = utf16SwapInChar;
    break;
  case utf8Encoding:
    f->string.charOut = utf8OutChar;
    f->string.charIn = utf8InChar;
    break;
  default:
    f->string.charOut = rawOutChar;
    f->string.charIn = rawInChar;
    break;
  }  
#endif
}

static retCode stringAtEof(ioPo io)
{
  stringPo f = O_STRING(io);

  if(f->string.pos<f->string.len)
    return Ok;
  else
    return Eof;
}

static retCode stringInReady(ioPo io)
{
  stringPo f = O_STRING(io);

  if(f->string.pos<f->string.len)
    return Ok;
  else
    return Eof;
}

static retCode stringOutReady(ioPo io)
{
  stringPo f = O_STRING(io);

  if(f->string.pos<f->string.len)
    return Ok;
  else{
    if(f->string.resizeable)
      return Ok;
    else
      return Fail;
  }
}

static retCode stringFlusher(ioPo io,long count)
{
  return Ok;
}

static retCode stringClose(ioPo io)
{
  destroyObject(O_OBJECT(io)); /* this will get rid of all the string objects attributes */
  return Ok;
}

stringPo openInStr(uniChar *buffer,long len,ioEncoding encoding)
{
  uniChar strName[] = {'<','s','t','r','>',0};
  return  O_STRING(newObject(stringClass,strName,encoding,buffer,len,ioREAD,False));
}

stringPo openStrInput(string name,uniChar *buffer,long len,ioEncoding encoding)
{
  return  O_STRING(newObject(stringClass,name,encoding,buffer,len,ioREAD,False));
}

retCode rewindStr(stringPo in)
{
  in->string.pos = 0;
  in->io.inBpos = in->io.inCpos = 0;
  return Ok;
}

stringPo openOutStr(ioEncoding encoding)
{
  uniChar strName[] = {'<','s','t','r','>',0};
  uniChar *buffer = (uniChar *)malloc(sizeof(uniChar)*128);

  return O_STRING(newObject(stringClass,strName,encoding,buffer,128,ioWRITE,True));
}

stringPo openStrOutput(uniChar *name,ioEncoding encoding)
{
  int len = 128;			/* initial length of buffer */
  uniChar *buffer = (uniChar *)malloc(sizeof(uniChar)*len);
  return O_STRING(newObject(stringClass,name,encoding,buffer,len,ioWRITE,True));
}

stringPo openBufferStr(uniChar *buffer,long len,ioEncoding encoding)
{
  uniChar strName[] = {'<','s','t','r','>',0};

  return O_STRING(newObject(stringClass,strName,encoding,buffer,len,ioWRITE,False));
}

stringPo openIoStr(ioEncoding encoding)
{
  uniChar strName[] = {'<','s','t','r','>',0};
  uniChar *buffer = (uniChar *)malloc(sizeof(uniChar)*128);

  return O_STRING(newObject(stringClass,strName,encoding,buffer,128,ioREAD|ioWRITE,True));
}

retCode emptyOutStr(stringPo str)
{
  if(str->string.pos>0)
    return Fail;
  else
    return Ok;
}

uniChar *getStrText(stringPo s,long *len)
{
  *len = s->string.pos;

  return s->string.buffer;
}

long getStrPos(stringPo s)
{
  return s->string.pos;
}

uniChar *strMsg(uniChar *buffer,long len,char *fmt,...)
{
  stringPo f = openBufferStr(buffer,len,utf16Encoding);

  va_list args;			/* access the generic arguments */
  va_start(args,fmt);		/* start the variable argument sequence */

  __voutMsg(O_IO(f),(unsigned char*)fmt,args);	/* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f),'\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}

uniChar *strAppend(uniChar *buffer,long len,char *fmt,...)
{
  uniChar *buff = uniEndStr(buffer);
  long blen = len - (buff-buffer);

  stringPo f = openBufferStr(buff,blen,utf16Encoding);

  va_list args;			/* access the generic arguments */
  va_start(args,fmt);		/* start the variable argument sequence */

  __voutMsg(O_IO(f),(unsigned char*)fmt,args);	/* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f),'\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}



// General error reporting function
retCode ioErrorMsg(ioPo io,char *fmt,...)
{
  stringPo f = O_STRING(newObject(stringClass,"fixed",utf16Encoding,io->io.msg,NumberOf(io->io.msg),ioWRITE,False));

  va_list args;			/* access the generic arguments */
  va_start(args,fmt);		/* start the variable argument sequence */

  __voutMsg(O_IO(f),(unsigned char*)fmt,args);	/* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f),'\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return Error;
}
