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
typedef progress (*ioStatusProc)(ioPo f);
typedef retCode (*filterProc)(ioPo f, void *cl);
typedef retCode (*flushProc)(ioPo f, long count);
typedef retCode (*byteOutProc)(ioPo f, byte *cl, integer count, integer *actual);
typedef retCode (*byteInProc)(ioPo f, byte *ch, integer count, integer *actual);
typedef retCode (*ioReadyProc)(ioPo f,integer count);

typedef struct {
  byteInProc read;                      /* procedure to read a byte */
  byteOutProc write;                    /* procedure to write a byte */
  retCode (*backByte)(ioPo io, byte b);  /* procedure to put a byte back in the file */

  ioReadyProc inputReady;                // Are we immediately able to read XX bytes?
  ioReadyProc outputReady;

  ioProc isAtEof;                        /* Are we at the end of file? */
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
  ioDirection mode;                         /* Mode that file is opened for */
  ioEncoding encoding;                  /* What is the mode for string encoding */
  integer inBpos;                        /* Byte in counter */
  integer inCpos;                        /* Character in counter */

  integer outBpos;                       /* Byte out counter */

  long currColumn;                      /* No. characters since last lf */
  retCode status;                       /* current status of the file object */
} IoPart;

typedef struct io_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;                   // Lock part of object
  IoPart io;                            /* Io level of io object */
} IoObject;

/* IO management procedures */

retCode setBufferStatus(ioPo f, retCode status);

#endif

