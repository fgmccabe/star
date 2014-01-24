/* 
   I/O handling top-level
   (c) 1994-2010 F.G. McCabe

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
#ifndef _IO_LIB_H_
#define _IO_LIB_H_

#include "retcode.h"
#include "integer.h"
#include "number.h"
#include "logical.h"
#include "object.h"

#undef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof(a[0]))

/* Status return type ... most API functions return one of these */
typedef enum {ioFile, ioChar, ioBlock, ioDir, ioPipe, ioString, ioLog, ioUDP } ioType;
typedef enum {ioNULL=000,ioREAD=001,ioWRITE=002} ioState;

typedef struct _io_object_ *ioPo;
extern classPo ioClass;

typedef unsigned char byte;

#include "unicode.h"
#include "formio.h"                     /* access the formatted I/O functions */

#ifndef MAXLINE
#define MAXLINE 1024		/* Size of a standard buffer */
#endif

#ifndef MAXFILELEN
#define MAXFILELEN 256		/* Maximum length of a file name */
#endif

void initIo(void);                      /* Initialize I/O system */
void closeIo(void);                     /* Close down the I/O system */

byte inB(ioPo f);                     /* read a single byte */
retCode inByte(ioPo f,byte *ch);      /* read a single byte -- with status */
retCode inBytes(ioPo f,byte *buffer,long len,long *actual); /* read a block of bytes */
retCode putBackByte(ioPo f,byte b);

uniChar *ioMessage(ioPo f);

retCode inChar(ioPo f,uniChar *ch);     /* read a character */
retCode unGetChar(ioPo f,uniChar ch);   /* put a single character back */
retCode inLine(ioPo f,uniChar *buffer,long len,long *actual,uniChar *term);

retCode inBlock(ioPo f,byte *buffer,long len);
retCode inBytes(ioPo f,byte *buffer,long len,long *act);

retCode pushBack(ioPo f,uniChar *str,unsigned long len);
retCode skipBlanks(ioPo f);

retCode outByte(ioPo f,byte c);
retCode outChar(ioPo f,uniChar ch);
retCode outBlock(ioPo f,byte *data,long len);
retCode outBytes(ioPo f,byte *data,long len,long *actual);

retCode outText(ioPo f,uniChar *text,unsigned long len);
retCode outCText(ioPo f,char *text,unsigned long len);
retCode outStr(ioPo f,char *str);
long outColumn(ioPo f);			/* return number of chars since lf */

retCode closeFile(ioPo f);            /* generic file closer */
retCode flushFile(ioPo f);            /* generic file flush */
retCode preFlushFile(ioPo f,int count); /* file flush */
void flushOut(void);                    /* flush all files */

retCode isFileOpen(ioPo f);
retCode isReadingFile(ioPo f);
retCode isWritingFile(ioPo f);
retCode isInReady(ioPo f);
retCode isOutReady(ioPo f);
retCode isFileAtEof(ioPo f);
retCode fileStatus(ioPo f);

long ioRefCount(ioPo f);
void incRefCount(ioPo f);
void decRefCount(ioPo f);

uniChar *fileName(ioPo f);
ioState fileMode(ioPo f);
long inBPos(ioPo f);
long inCPos(ioPo f);
long outBPos(ioPo f);
long outCPos(ioPo f);
retCode ioSeek(ioPo f,long pos);

typedef retCode (*ioPropertyFun)(ioPo f,void *k,void *v,void *c); /* Processing func */

retCode setFileProperty(ioPo f,void *key,void *val);
void removeFileProperty(ioPo f,void *key);
retCode fileProperty(ioPo f,void *key,void **val);
retCode processFileProperties(ioPo f,ioPropertyFun fn,void *c);
retCode processAllFileProperties(ioPropertyFun f,void *c);

retCode ioErrorMsg(ioPo io,char *fmt,...);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_IO(c) ((ioPo)(checkCast((c),ioClass)))
#else
#define O_IO(c) ((ioPo)(c))
#endif

#endif
