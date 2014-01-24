/* 
   I/O handling library, common framework module
   This is an abstract class -- cannot be instantiated by itself
 
   (c) 1994-2011 F.G. McCabe

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
   
   Contact: Francis McCabe <fmccabe@gmail.com>
*/ 

#include "config.h"		/* Invoke configuration header */
#include "ioP.h"
#include "fileP.h"
#include "uriP.h"
#include "pool.h"

#include <stdlib.h>
#include <assert.h>
#include <string.h>

static void initIoClass(classPo class,classPo request);
static void inheritIo(classPo class,classPo request);
static void ioClose(objectPo o);
static void IoInit(objectPo list,va_list *args);

static retCode nullInChar(ioPo f,uniChar *ch);
static retCode nullInBytes(ioPo f,byte *ch,long count,long *actual);
static retCode nullOutChar(ioPo f,uniChar ch);
static retCode nullOutBytes(ioPo f,byte *b,long count,long *actual);
static retCode nullOutByte(ioPo f,byte b);

static retCode nullEof(ioPo f);
static retCode nullReady(ioPo f);
static retCode nullFlusher(ioPo f,long count);
static retCode nullSeek(ioPo f,long count);
static retCode nullClose(ioPo f);

IoClassRec IoClass = {
  {
    (classPo)&ManagedClass,               /* parent class is managed object */
    "io",                                 /* this is the io class */
    inheritIo,                            /* deal with inheritance */
    initIoClass,                          /* IO class initializer */
    O_INHERIT_DEF,                        /* IO object element creation */
    ioClose,                              /* IO objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    IoInit,                               /* initialization of an Io buffer */
    sizeof(IoObject),         /* min size of an io record -- should never use */
    NULL,				/* pool of values for this class */
    PTHREAD_ONCE_INIT,			  /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL                                /* initially no elements managed */
  },
  {
    nullInChar,			   /* inChar, abstract for the io class  */
    nullOutChar,		   /* outChar, abstract for the io class  */
    nullOutChar,		   /* ungetChar, abstract for the io class  */
    nullInBytes,		   /* inByte, abstract for the io class  */
    nullOutBytes,		   /* outByte, abstract for the io class  */
    nullOutByte,                   /* putbackByte, abstract for the io class  */
    nullEof,			   /* are we at end of file? */
    nullReady,			   /* readyIn, abstract for the io class  */
    nullReady,			   /* readyOut, abstract for the io class  */
    nullFlusher,		   /* flush, abstract for the io class  */
    nullSeek,			   /* seek, abstract for the io class */
    nullClose			   /* close, abstract for the io class  */
  }
};

classPo ioClass = (classPo)&IoClass;

static void inheritIo(classPo class,classPo request)
{
  IoClassRec *req = (IoClassRec*)request;
  IoClassRec *template = (IoClassRec*)request;
  logical done = False;

  while(!done){
    done = True;
    if(req->ioPart.inChar==O_INHERIT_DEF){
      if(template->ioPart.inChar!=O_INHERIT_DEF)
	req->ioPart.inChar = template->ioPart.inChar;
      else
	done = False;
    }

    if(req->ioPart.outChar==O_INHERIT_DEF){
      if(template->ioPart.outChar!=O_INHERIT_DEF)
	req->ioPart.outChar = template->ioPart.outChar;
      else
	done = False;
    }

    if(req->ioPart.ungetChar==O_INHERIT_DEF){
      if(template->ioPart.ungetChar!=O_INHERIT_DEF)
	req->ioPart.ungetChar = template->ioPart.ungetChar;
      else
	done = False;
    }

    if(req->ioPart.inBytes==O_INHERIT_DEF){
      if(template->ioPart.inBytes!=O_INHERIT_DEF)
	req->ioPart.inBytes = template->ioPart.inBytes;
      else
	done = False;
    }

    if(req->ioPart.backByte==O_INHERIT_DEF){
      if(template->ioPart.backByte!=O_INHERIT_DEF)
	req->ioPart.backByte = template->ioPart.backByte;
      else
	done = False;
    }

    if(req->ioPart.outBytes==O_INHERIT_DEF){
      if(template->ioPart.outBytes!=O_INHERIT_DEF)
	req->ioPart.outBytes = template->ioPart.outBytes;
      else
	done = False;
    }

    if(req->ioPart.isAtEof==O_INHERIT_DEF){
      if(template->ioPart.isAtEof!=O_INHERIT_DEF)
	req->ioPart.isAtEof = template->ioPart.isAtEof;
      else
	done = False;
    }

    if(req->ioPart.inReady==O_INHERIT_DEF){
      if(template->ioPart.inReady!=O_INHERIT_DEF)
	req->ioPart.inReady = template->ioPart.inReady;
      else
	done = False;
    }

    if(req->ioPart.outReady==O_INHERIT_DEF){
      if(template->ioPart.outReady!=O_INHERIT_DEF)
	req->ioPart.outReady = template->ioPart.outReady;
      else
	done = False;
    }

    if(req->ioPart.flush==O_INHERIT_DEF){
      if(template->ioPart.flush!=O_INHERIT_DEF)
	req->ioPart.flush = template->ioPart.flush;
      else
	done = False;
    }

    if(req->ioPart.seek==O_INHERIT_DEF){
      if(template->ioPart.seek!=O_INHERIT_DEF)
	req->ioPart.seek = template->ioPart.seek;
      else
	done = False;
    }

    if(req->ioPart.close==O_INHERIT_DEF){
      if(template->ioPart.close!=O_INHERIT_DEF)
	req->ioPart.close = template->ioPart.close;
      else
	done = False;
    }

    template = (IoClassRec*)(template->objectPart.parent);
  }
}

static pthread_once_t ioOnce = PTHREAD_ONCE_INIT;

static void initIoEtc(void)
{
  atexit(closeIo);                      /* set up general closer for exit */
  initRecursiveMutex(&ioClass->mutex);
  initUri();
}
  
static void initIoClass(classPo class,classPo request)
{
  pthread_once(&ioOnce,initIoEtc);
}

static ioPo activeSet = NULL;

static void IoInit(objectPo o,va_list *args){
  ioPo f = O_IO(o);
  uniChar *name = va_arg(*args,uniChar*);

  lockClass(ioClass);

  if(activeSet==NULL)
    activeSet = f->io.next = f->io.prev = f;
  else{
    f->io.next = activeSet;
    f->io.prev = activeSet->io.prev;
    activeSet->io.prev->io.next = f;
    activeSet->io.prev = f;
    activeSet = f;
  }

  f->io.refCount = 1;                   /* one reference by default */
  uniCpy(f->io.filename,NumberOf(f->io.filename),name);
  f->io.status = Ok;
  f->io.inBpos = 0;
  f->io.inCpos = 0;
  f->io.outBpos = 0;
  f->io.outCpos = 0;
  f->io.currColumn = 0;

  unlockClass(ioClass);
}

static void ioClose(objectPo o)
{
  /*  if(isObjectOfClass(O_OBJECT(o),fileClass))
    configureIo(O_FILE(o),turnOnBlocking);
  closeFile(O_IO(o));
  */
}

void closeIo(void)
{
  flushOut();
  
  lockClass(ioClass);
  while(activeSet!=NULL){
    if(isObjectOfClass(O_OBJECT(activeSet),fileClass))
      configureIo(O_FILE(activeSet),turnOnBlocking);
    closeFile(activeSet);
  }
  unlockClass(ioClass);
}

/* Byte level input on Io buffers */

byte inB(ioPo f)
{
  byte b;
  retCode ret = inByte(f,&b);
  if(ret==Ok)
    return b;
  else{
    ioErrorMsg(f,"problem in reading a byte from %U",fileName(f));
    return 0;
  }
}

retCode inByte(ioPo f,byte *b)
{
  byte buff[1];
  long act;
  retCode ret = inBytes(f,&buff[0],1,&act);

  if(ret==Ok){
    if(act==1){
      *b = buff[0];
      return Ok;
    }
    else
      return Fail;
  }
  return ret;
}

retCode inBytes(ioPo f,byte *ch,long count,long *actual)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.inBytes(f,ch,count,actual);
  f->io.inBpos+=*actual;
  unlock(o);

  return ret;
}

retCode putBackByte(ioPo f,byte b)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.backByte(f,b);

  if(ret==Ok)
    f->io.inBpos--;

  unlock(o);
  return ret;
}

/* Byte level output */

retCode outBytes(ioPo f,byte *data,long len,long *actual)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.outBytes(f,data,len,actual);
  f->io.outBpos+=*actual;

  unlock(o);
  return ret;
}

retCode outBlock(ioPo f,byte *data,long len)
{
  long actual;
  retCode ret = outBytes(f,data,len,&actual);

  if(ret==Ok && len!=actual)
    return ioErrorMsg(f,"couldnt write block of %d bytes properly to %U",len,fileName(f));
  else
    return ret;
}

retCode outByte(ioPo f,byte c)
{
  byte buff[1] = {c};
  long len = NumberOf(buff);
  retCode ret = outBytes(f,&buff[0],len,&len);

  if(ret==Ok && len!=NumberOf(buff))
    return ioErrorMsg(f,"couldnt write byte properly to %U",fileName(f));
  else
    return ret;
}

 
/* Character level input */
retCode inChar(ioPo f,uniChar *ch)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.inChar(f,ch);
  if(ret==Ok)
    f->io.inCpos++;
  unlock(o);

  return ret;
}

retCode unGetChar(ioPo f,uniChar ch)   /* put a single character back */
{
  if(ch!=uniEOF){
    objectPo o = O_OBJECT(f);
    retCode ret;

    lock(o);
    ret = ((IoClassRec*)f->object.class)->ioPart.ungetChar(f,ch);
    if(ret==Ok){
      f->io.inCpos--;
    }
    unlock(o);

    return ret;
  }
  else
    return Eof;
}

// Push a string back into the input channel
retCode pushBack(ioPo f,uniChar *str,unsigned long len)
{
  if(f!=NULL){
    int i;
    retCode ret = Ok;

    for(i=0;ret==Ok && i<len;i++)
      ret = unGetChar(f,str[i]);

    return ret;
  }
  else
    return Error;
}

/*
 * read a line ... up to a terminating character 
 * len should be at least 2 ... one for the final NULL byte
 */
retCode inLine(ioPo f,uniChar *buffer,long len,long *actual,uniChar *term)
{
  retCode stat = Ok;
  long tlen = uniStrLen(term);
  unsigned long act = 0;
  objectPo o = O_OBJECT(f);

  lock(o);

  if((f->io.mode&ioREAD)!=0){
    while(stat==Ok && --len>0){ /* we need at least one char for the NULL */
      uniChar ch;
      stat = inChar(f,&ch);

      if(stat==Ok){
	if(uniSearch(term,tlen,ch)==NULL){ /* have we found a terminating byte? */
          act++;
	  *buffer++=ch;
        }
	else
	  break;
      }
    }

    if(len>0){
      *buffer='\0';
      act++;
    }

    *actual = act;
  }
  else
    stat = Error;

  unlock(o);
  return stat;
}
   
/* Character-level output */

retCode outChar(ioPo f,uniChar ch)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.outChar(f,ch);
  if(ret==Ok)
    f->io.outCpos++;
  if(ch=='\n')
    f->io.currColumn = 0;
  else
    f->io.currColumn++;

  unlock(o);
  return ret;
}

retCode outText(ioPo f,uniChar *text,unsigned long len)
{
  int i;
  retCode ret = Ok;
  objectPo o = O_OBJECT(f);

  lock(o);

  for(i=0;ret==Ok && i<len;i++)
    ret = ((IoClassRec*)f->object.class)->ioPart.outChar(f,text[i]);

  unlock(o);
  return ret;
}

retCode outCText(ioPo f,char *text,unsigned long len)
{
  int i;
  retCode ret = Ok;
  objectPo o = O_OBJECT(f);

  lock(o);

  for(i=0;ret==Ok && i<len;i++)
    ret = ((IoClassRec*)f->object.class)->ioPart.outChar(f,text[i]);

  unlock(o);
  return ret;
}

retCode outStr(ioPo f,char *str)
{
  retCode ret = Ok;
  objectPo o = O_OBJECT(f);

  lock(o);

  while(ret==Ok && *str!='\0')
    ret = ((IoClassRec*)f->object.class)->ioPart.outChar(f,*str++);

  unlock(o);
  return ret;
}

retCode flushFile(ioPo f)               /* generic file flush */
{ 
  objectPo o = O_OBJECT(f);
  retCode ret = Ok;

  lock(o);

  if(isWritingFile(f)==Ok)
    ret = ((IoClassRec*)f->object.class)->ioPart.flush(f,0);

  unlock(o);
  return ret;
}

retCode preFlushFile(ioPo f,int count) /* file flush */
{ 
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.flush(f,count);
  unlock(o);

  return ret;
}

void flushOut(void)                     /* flush all files */
{
  lockClass(ioClass);

  if(activeSet!=NULL){
    ioPo f = activeSet;

    do{
      f = f->io.next;
      if((f->io.mode&ioWRITE)!=0){

	objectPo o = O_OBJECT(f);
	lock(o);

	while(flushFile(f)==Fail)
          ;
	unlock(o);
      }
    }while(f!=activeSet);
  }
  unlockClass(ioClass);
}

retCode ioSeek(ioPo f,long count)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)f->object.class)->ioPart.seek(f,count);
  unlock(o);
  return ret;
}

/* File opening is specific to the type of file being opened, 
 * but all files can be closed using the same function
 */

retCode closeFile(ioPo f)
{
  objectPo o = O_OBJECT(f);
  retCode ret = Ok;

  lock(o);

  if(--f->io.refCount<=0){
    while(flushFile(f)==Fail);
    //    clearFileProperties(f);     // clear out any attached properties

    lockClass(ioClass);

    if(f==activeSet){
      if(f->io.next==f && f->io.prev==f)
	activeSet=NULL;		/* no more active files */
      else
	activeSet = f->io.next;	/* move the base pointer along */
    }

    f->io.next->io.prev = f->io.prev;
    f->io.prev->io.next = f->io.next;

    unlockClass(ioClass);

    ret = ((IoClassRec*)f->object.class)->ioPart.close(f);
  }

  unlock(o);
  return ret;
}


retCode skipBlanks(ioPo f)
{
  retCode stat = Error;
  objectPo o = O_OBJECT(f);

  lock(o);

  if((f->io.mode&ioREAD)!=0){
    if(f->io.status!=Ok)
      stat = f->io.status;              /* End of file is a `char' */
    else{
      uniChar ch;

      while((stat=inChar(f,&ch))==Ok && isZsChar(ch))
        ;
      if(stat==Ok)
        stat = unGetChar(f,ch);
    }
  }

  unlock(o);
  return stat;
}

retCode outB(ioPo f,byte c)
{
  byte buff[1]={c};
  long act;
  retCode ret = outBytes(f,&buff[0],NumberOf(buff),&act);
 
  if(ret==Ok)
    if(act!=1)
      return Fail;
  return ret;
}

void triggerIo(filterProc filter,void *cl)
{
  lockClass(ioClass);

  if(activeSet!=NULL){
    ioPo f = activeSet;

    do{
      f = f->io.next;

      filter(f,cl);
    }while(f!=activeSet);
  }

  unlockClass(ioClass);
}


static retCode nullInChar(ioPo f,uniChar *ch)
{
  return Error;
}

static retCode nullInBytes(ioPo f,byte *ch,long count,long *actual)
{
  return Error;
}

static retCode nullOutChar(ioPo f,uniChar ch)
{
  return Error;
}

static retCode nullOutBytes(ioPo f,byte *b,long count,long *actual)
{
  return Error;
}

static retCode nullOutByte(ioPo f,byte b)
{
  return Error;
}

static retCode nullFlusher(ioPo f,long count)
{
  return Ok;
}

static retCode nullSeek(ioPo f,long count)
{
  return Ok;
}

static retCode nullClose(ioPo f)
{
  return Ok;
}

static retCode nullEof(ioPo f)
{
  return Error;
}

static retCode nullReady(ioPo f)
{
  return Error;
}

/* Access macros & functions */
retCode wasFileAtEof(ioPo f)		/* Ok if at end of file */
{
  retCode ret = f->io.status;
  objectPo o = O_OBJECT(f);

  lock(o);

  if(f->io.status==Ok){
    uniChar ch;                         /* we will attempt to read a char */
    ret = inChar(f,&ch);

    if(ret==Ok)
      unGetChar(f,ch);
  }
  else
    ret = f->io.status;

  unlock(o);
  return ret;
}

retCode isFileAtEof(ioPo f)		/* Ok if at end of file */
{
  objectPo o = O_OBJECT(f);

  lock(o);

  retCode ret = ((IoClassRec*)f->object.class)->ioPart.isAtEof(f);

  unlock(o);
  return ret;
}


retCode fileStatus(ioPo f)
{
  retCode ret;
  objectPo o = O_OBJECT(f);

  lock(o);
  ret = f->io.status;
  unlock(o);

  return ret;
}

retCode setBufferStatus(ioPo f,retCode status)
{
  objectPo o = O_OBJECT(f);

  lock(o);
  f->io.status = status;
  unlock(o);
  return status;
}

long ioRefCount(ioPo f)
{
  objectPo o = O_OBJECT(f);
  long count;

  lock(o);
  count = f->io.refCount;
  unlock(o);
  return count;
}

void incRefCount(ioPo f)
{
  objectPo o = O_OBJECT(f);

  lock(o);
  f->io.refCount++;
  unlock(o);
}

void decRefCount(ioPo f)
{
  objectPo o = O_OBJECT(f);

  lock(o);
  f->io.refCount--;
  unlock(o);
}

ioState fileMode(ioPo f)
{
  objectPo o = O_OBJECT(f);
  ioState mode;

  lock(o);
  mode = f->io.mode;
  unlock(o);
  return mode;
}

uniChar *fileName(ioPo f)
{
  return f->io.filename;
}

long inBPos(ioPo f)
{  
  objectPo o = O_OBJECT(f);
  long bPos;

  lock(o);
  bPos = f->io.inBpos;
  unlock(o);
  return bPos;
}

long inCPos(ioPo f)
{  
  objectPo o = O_OBJECT(f);
  long cPos;

  lock(o);
  cPos = f->io.inCpos;
  unlock(o);
  return cPos;
}

long outBPos(ioPo f)
{  
  objectPo o = O_OBJECT(f);
  long bPos;

  lock(o);
  bPos = f->io.outBpos;
  unlock(o);
  return bPos;
}

long outCPos(ioPo f)
{  
  objectPo o = O_OBJECT(f);
  long cPos;

  lock(o);
  cPos = f->io.outCpos;
  unlock(o);
  return cPos;
}

long outColumn(ioPo f)
{  
  objectPo o = O_OBJECT(f);
  long col;

  lock(o);
  col = f->io.currColumn;
  unlock(o);
  return col;
}

retCode isFileOpen(ioPo f)
{
  ioState mode = fileMode(f);

  if(mode!=ioNULL)
    return Ok;
  else
    return Fail;
}

retCode isReadingFile(ioPo f)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  if((fileMode(f)&ioREAD)!=0)
    ret = Ok;
  else
    ret = Fail;
  unlock(o);
  return ret;
}

retCode isWritingFile(ioPo f)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  if((fileMode(f)&ioWRITE)!=0)
    ret = Ok;
  else
    ret = Fail;
  unlock(o);
  return ret;
}


/* test that a file is ready without actually reading anything */
retCode isInReady(ioPo f)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)(f->object.class))->ioPart.inReady(f);
  unlock(o);

  return ret;
}

retCode isOutReady(ioPo f)
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(o);
  ret = ((IoClassRec*)(f->object.class))->ioPart.outReady(f);
  unlock(o);
  return ret;
}

uniChar *ioMessage(ioPo f)
{
  return f->io.msg;
}
