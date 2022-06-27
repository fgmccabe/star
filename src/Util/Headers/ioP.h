/* 
  I/O handling library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe
*/

#ifndef _IO_P_H_
#define _IO_P_H_

#include "objectP.h"                    /* access object system */
#include "lockableP.h"
#include "io.h"
#include <stdarg.h>

typedef retCode (*ioProc)(ioPo f);
typedef logical (*ioStatusProc)(ioPo f);
typedef retCode (*filterProc)(ioPo f, void *cl);
typedef retCode (*flushProc)(ioPo f, long count);
typedef retCode (*byteOutProc)(ioPo f, byte *cl, integer count, integer *actual);
typedef retCode (*byteInProc)(ioPo f, byte *ch, integer count, integer *actual);
typedef retCode (*asyncByteInProc)(ioPo f, integer count, void *cl);
typedef retCode (*asyncByteOutProc)(ioPo f, byte *ch, integer count, void *cl);

typedef struct {
  byteInProc read;                      /* procedure to read a byte */
  asyncByteInProc async_read;           /* request an asynchronous read operation */
  byteOutProc write;                    /* procedure to write a byte */
  asyncByteOutProc async_write;         /* Request an asynchronous write operation */
  retCode (*backByte)(ioPo io, byte b);  /* procedure to put a byte back in the file */

  ioProc isAtEof;                        /* Are we at the end of file? */

  flushProc flush;                      /* Called when file is to be flushed */
  ioProc close;                         /* Called when file is to be closed */
} IoClassPartRec;

typedef struct io_class__ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                /* the io part of the class information */
} IoClassRec;

extern IoClassRec IoClass;              /* the standard pointer to an IO class record */

typedef struct io_part__ {
  char filename[MAXFILELEN];            /* File name */
  retCode status;                       /* current status of the io object */
  ioDirection mode;                         /* Mode that file is opened for */
  ioEncoding encoding;                  /* What is the mode for string encoding */
  integer inBpos;                        /* Byte in counter */
  integer inCpos;                        /* Character in counter */

  integer outBpos;                       /* Byte out counter */

  long currColumn;                      /* No. characters since last lf */

  ioPo prev;                            /* Previous file in open set */
  ioPo next;                            /* Next file in open set */
} IoPart;

typedef struct io_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;                   // Lock part of object
  IoPart io;                            /* Io level of io object */
} IoObject;

/* IO management procedures */

retCode setBufferStatus(ioPo f, retCode status);

#endif

