/*
  Byte String file handling functions 
  (c) 1994-2004 Imperial College & F.G. McCabe

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

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
*/

#include "config.h"		/* pick up standard configuration header */
#include "bytesP.h"
#include "formioP.h"
#include "utf.h"

#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

/* Set up the byte file class */

static void initByteClass(classPo class,classPo req);
static void ByteDestroy(objectPo o);
static void ByteInit(objectPo list,va_list *args);

static retCode byteInChar(ioPo f,uniChar *ch);
static retCode byteInBytes(ioPo f,byte *ch,long count,long *actual);
static retCode byteOutChar(ioPo f,uniChar ch);
static retCode byteUngetChar(ioPo f,uniChar ch);
static retCode byteOutBytes(ioPo f,byte *b,long count,long *actual);
static retCode byteBackByte(ioPo f,byte b);

static retCode byteAtEof(ioPo f);
static retCode byteInReady(ioPo f);
static retCode byteOutReady(ioPo f);
static retCode byteFlusher(ioPo f,long count);
static retCode byteSeek(ioPo f,long count);
static retCode byteClose(ioPo f);

static void setEncoding(bytePo f,ioEncoding encoding);

ByteClassRec ByteClass = {
  {
    (classPo)&IoClass,			  /* parent class is io object */
    "bytes",                              /* this is the byte class */
    NULL,
    initByteClass,                        /* Byte class initializer */
    O_INHERIT_DEF,                        /* Byte object element creation */
    ByteDestroy,                          /* Byte objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    ByteInit,                             /* initialization of a byte object */
    sizeof(ByteObject),                   /* size of a byte object */
    NULL,				  /* pool of byte values */
    PTHREAD_ONCE_INIT,			  /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL
  },
  {
    byteInChar,                         /* inChar  */
    byteOutChar,                        /* outChar  */
    byteUngetChar,                      /* ungetChar  */
    byteInBytes,                        /* inByte  */
    byteOutBytes,                       /* outBytes  */
    byteBackByte,                       /* backByte */
    byteAtEof,				/* atEof */
    byteInReady,                        /* readyIn  */
    byteOutReady,                       /* readyOut  */
    byteFlusher,                        /* flush  */
    byteSeek,                           /* seek */
    byteClose                           /* close  */
  }
};

classPo byteClass = (classPo)&ByteClass;

static void initByteClass(classPo class,classPo req)
{
}

// IO initialization should already be done at this point
static void ByteInit(objectPo o,va_list *args)
{
  bytePo f = O_BYTE(o);

  // Set up the buffer pointers
  f->byte.pos = 0;
  f->byte.buffer = va_arg(*args,byte *);
  f->byte.len = va_arg(*args,long);   /* set up the buffer */
  f->io.mode = va_arg(*args,ioState);   /* set up the access mode */
  setEncoding(f,va_arg(*args,ioEncoding));     /* set up the encoding */
  f->byte.resizeable = va_arg(*args,logical); /* is this byte resizeable? */
}

static void ByteDestroy(objectPo o)
{
  //  bytePo str = O_BYTE(o);
}

// Implement class byte functions
static retCode byteInChar(ioPo io,uniChar *ch)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos<f->byte.len){
    *ch=f->byte.buffer[f->byte.pos++];
    return Ok;
  }
  else
    return Eof;
}

static retCode byteUngetChar(ioPo io,uniChar ch)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos>0){
    f->byte.buffer[--f->byte.pos]=ch;
    f->io.status = Ok;
    return Ok;
  }
  else
    return Error;
}

static retCode byteInBytes(ioPo io,byte *ch,long count,long *actual)
{
  retCode ret = Ok;
  long remaining = count;
  bytePo f = O_BYTE(io);

  while(ret==Ok && remaining>0){
    if(f->byte.pos>=f->byte.len){
      if(remaining==count)
        ret = Eof;
      break;
    }
    else{
      *ch++=f->byte.buffer[f->byte.pos++];
      remaining--;
    }
  }
  *actual = count-remaining;

  return ret;
}

static retCode byteOutChar(ioPo io,uniChar ch)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos>=f->byte.len){
    if(f->byte.resizeable){
      long nlen = f->byte.len+(f->byte.len>>1); /* allow for some growth */
      byte *nbuff = realloc(f->byte.buffer,sizeof(byte)*nlen);
      if(nbuff!=NULL){
        f->byte.buffer = nbuff;
        f->byte.len = nlen;
      }
      else
        return ioErrorMsg(io,"could not allocate more space for byte");
    }
    else
      return Ok;                        /* we silently drop extra data */
  }

  f->byte.buffer[f->byte.pos++]=ch;
  return Ok;
}

static retCode byteOutBytes(ioPo io,byte *b,long count,long *actual)
{
  retCode ret = Ok;
  long remaining = count;

  while(ret==Ok && remaining>0){
    ret = byteOutChar(io,(uniChar)*b++);
    remaining--;
  }
  *actual = count-remaining;
  return ret;
}

static retCode byteBackByte(ioPo io,byte b)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos>0){
    f->byte.buffer[--f->byte.pos]=b;
    return Ok;
  }
  else
    return Error;
}

static void setEncoding(bytePo f,ioEncoding encoding)
{
  f->byte.encoding = encoding;

  switch(encoding){
  case utf16Encoding:
    f->byte.charOut = utf16OutChar;
    f->byte.charIn = utf16InChar;
    break;
  case utf16EncodingSwap:
    f->byte.charOut = utf16SwapOutChar;
    f->byte.charIn = utf16SwapInChar;
    break;

  case utf8Encoding:
    f->byte.charOut = utf8OutChar;
    f->byte.charIn = utf8InChar;
    break;
  default:
    f->byte.charOut = rawOutChar;
    f->byte.charIn = rawInChar;
    break;
  }  
}

static retCode byteAtEof(ioPo io)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos<f->byte.len)
    return Ok;
  else
    return Eof;
}

static retCode byteInReady(ioPo io)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos<f->byte.len)
    return Ok;
  else
    return Eof;
}

static retCode byteOutReady(ioPo io)
{
  bytePo f = O_BYTE(io);

  if(f->byte.pos<f->byte.len)
    return Ok;
  else{
    if(f->byte.resizeable)
      return Ok;
    else
      return Fail;
  }
}

static retCode byteFlusher(ioPo io,long count)
{
  return Ok;
}

static retCode byteSeek(ioPo io,long count)
{
  bytePo f = O_BYTE(io);

  if(count>=0 && count<f->byte.len){    /* can only seek within the current buffer */
    f->byte.pos = count;
    return Ok;
  }
  else
    return Fail;
}

static retCode byteClose(ioPo io)
{
  destroyObject(O_OBJECT(io)); /* this will get rid of all the byte objects attributes */
  return Ok;
}

ioPo openInByteStr(byte *buffer,long len,ioEncoding encoding)
{
  uniChar strName[] = {'<','b','y','t','e','>',0};
  return  O_IO(newObject(byteClass,strName,buffer,len,ioREAD,encoding,False));
}

retCode rewindByteStr(bytePo in)
{
  in->byte.pos = 0;
  return Ok;
}

ioPo openOutByteStr(ioEncoding encoding)
{
  uniChar strName[] = {'<','b','y','t','e','s','t','r','>',0};
  byte *buffer = (byte *)malloc(sizeof(byte)*128);

  return O_IO(newObject(byteClass,strName,buffer,128,ioWRITE,encoding,True));
}

retCode emptyByteStr(bytePo str)
{
  if(str->byte.pos>0)
    return Fail;
  else
    return Ok;
}

byte *getByteStr(bytePo s,long *len)
{
  *len = s->byte.pos;

  return s->byte.buffer;
}

long getBytePos(bytePo s)
{
  return s->byte.pos;
}

byte *byteMsg(byte *buffer,long len,char *fmt,...)
{
  bytePo f = O_BYTE(newObject(byteClass,"fixed",buffer,len,(ioREAD|ioWRITE),utf8Encoding,False));

  va_list args;			/* access the generic arguments */
  va_start(args,fmt);		/* start the variable argument sequence */

  __voutMsg(O_IO(f),(unsigned char*)fmt,args);	/* Display into the byte buffer */

  va_end(args);
  outChar(O_IO(f),'\0');                /* Terminate the byte */

  closeFile(O_IO(f));
  return buffer;
}

