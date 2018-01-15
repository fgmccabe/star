/* 
  I/O handling library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_P_H_
#define _IO_P_H_

#include "objectP.h"                    /* access object system */
#include "lockableP.h"
#include "io.h"
#include <stdarg.h>

typedef struct _property_ *propertyPo;

typedef retCode (*ioProc)(ioPo f);
typedef retCode (*filterProc)(ioPo f,void *cl);
typedef retCode (*flushProc)(ioPo f,long count);
typedef retCode (*seekProc)(ioPo f,integer count);
typedef retCode (*byteOutProc)(ioPo f,byte *cl,integer count,integer *actual);
typedef retCode (*byteInProc)(ioPo f,byte *ch,integer count,integer *actual);
typedef retCode (*markProc)(ioPo f,integer *mark);
typedef retCode (*resetProc)(ioPo f,integer mark);

typedef struct {
  byteInProc read;                      /* procedure to read a byte */
  byteOutProc write;                    /* procedure to write a byte */
  retCode (*backByte)(ioPo io,byte b);  /* procedure to put a byte back in the file */

  markProc mark;                        /* Attempt to place a backtrackable mark */
  resetProc reset;                      /* Reset to previous mark */

  ioProc isAtEof;			                  /* Are we at the end of file? */

  ioProc inReady;                       /* Called to determine if file has input */
  ioProc outReady;                      /* Called to determine if file can output */

  flushProc flush;                      /* Called when file is to be flushed */
  seekProc seek;                        /* called when seeking */
  ioProc close;                         /* Called when file is to be closed */
} IoClassPartRec;
  
typedef struct _io_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                /* the io part of the class information */
} IoClassRec;

extern IoClassRec IoClass;              /* the standard pointer to an IO class record */

typedef struct _io_part_{
  char filename[MAXFILELEN];            /* File name */
  retCode status;                       /* current status of the io object */
  ioDirection mode;                         /* Mode that file is opened for */
  ioEncoding encoding;			            /* What is the mode for string encoding */
  integer inBpos;                        /* Byte in counter */
  integer inCpos;                        /* Character in counter */

  integer outBpos;                       /* Byte out counter */

  long currColumn;			                /* No. characters since last lf */

  ioPo prev;                            /* Previous file in open set */
  ioPo next;                            /* Next file in open set */
} IoPart;

typedef struct _io_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;                   // Lock part of object
  IoPart io;                            /* Io level of io object */
} IoObject;

/* IO management procedures */

retCode setBufferStatus(ioPo f,retCode status);

#endif

