/* 
  Pipe library (private header)
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
*/ 

#ifndef IO_PIPE_P_H_
#define IO_PIPE_P_H_

#include "config.h"
#include "fileP.h"
#include "pipe.h"
#include <stdarg.h>

typedef struct {
} PipeClassPartRec;

typedef struct pipe_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;              /* the io part of the class information */
  FileClassPartRec filePart;
  PipeClassPartRec pipePart;
} PipeClassRec;

extern PipeClassRec PipeClass; /* the standard pointer to a pipe class record */

typedef struct pipe_part_{
  int childProcess;
  pipePo next,prev;                     /* pipes are connected to each other */
} PipePart;

typedef struct pipe_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;                   // Lock part of object
  IoPart io;                            /* Io level of io object */
  FilePart file;                        /* File level of file object */
  PipePart pipe;                        /* Pipeet level part of the file */
} PipeObject;

#ifdef VERIFY_OBJECT
#define O_PIPE(c) ((pipePo)(checkCast((c),(classPo)&PipeClass)))
#else
#define O_PIPE(c) ((pipePo)(c))
#endif

#endif
