/* 
   I/O handling library (private header)
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

#ifndef _IO_P_H_
#define _IO_P_H_

#include "managedP.h"                    /* access object system */
#include "io.h"
#include <stdarg.h>

typedef struct _property_ *propertyPo;

typedef retCode (*ioProc)(ioPo f);
typedef retCode (*filterProc)(ioPo f,void *cl);
typedef retCode (*flushProc)(ioPo f,long count);
typedef retCode (*seekProc)(ioPo f,long count);
typedef retCode (*charOutProc)(ioPo f,uniChar cl);
typedef retCode (*charInProc)(ioPo f,uniChar *ch);
typedef retCode (*byteOutProc)(ioPo f,byte *cl,long count,long *actual);
typedef retCode (*byteInProc)(ioPo f,byte *ch,long count,long *actual);

typedef struct {
  charInProc inChar;                    /* procedure to read a character */
  charOutProc outChar;                  /* procedure to write a character */
  charOutProc ungetChar;                /* procedure to unget a character */

  byteInProc inBytes;                    /* procedure to read a byte */
  byteOutProc outBytes;                  /* procedure to write a byte */
  retCode (*backByte)(ioPo io,byte b); /* procedure to put a byte back in the file */

  ioProc isAtEof;			/* Are we at the end of file? */

  ioProc inReady;                    /* Called to determine if file has input */
  ioProc outReady;                  /* Called to determine if file can output */

  flushProc flush;                      /* Called when file is to be flushed */
  seekProc seek;                        /* called when seeking */
  ioProc close;                         /* Called when file is to be closed */
} IoClassPartRec;
  
typedef struct _io_class_ {
  ObjectClassRec objectPart;
  ManagedClassPartRec managedPart;      /* IO objects are managed */
  IoClassPartRec ioPart;              /* the io part of the class information */
} IoClassRec;

extern IoClassRec IoClass;      /* the standard pointer to an IO class record */

typedef struct _io_part_{
  uniChar filename[MAXFILELEN];         /* File name */
  retCode status;                       /* current status of the io object */
  uniChar msg[MAXFILELEN];		/* used to record messages */
  ioState mode;                         /* Mode that file is opened for */
  long inBpos;                          /* Byte in counter */
  long inCpos;                          /* Character in counter */

  long outBpos;                         /* Byte out counter */
  long outCpos;                         /* Character out counter */

  long currColumn;			/* No. characters since last lf */

  long refCount;                        /* Reference count */
  ioPo prev;                          /* Previous file in open set */
  ioPo next;                          /* Next file in open set */
} IoPart;

typedef struct _io_object_ {
  ObjectRec object;                     /* object level of the io structure */
  ManagedRec managed;                   /* The managed part of the io structure */
  IoPart io;                            /* Io level of io object */
} IoObject;

/* IO management procedures */

retCode setBufferStatus(ioPo f,retCode status);

static long inline min(long a,long b)
{
  if(a<b)
    return a;
  else
    return b;
}

#endif

