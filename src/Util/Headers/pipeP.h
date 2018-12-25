/* 
   Pipe library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_PIPE_P_H_
#define _IO_PIPE_P_H_

#include "config.h"
#include "fileP.h"
#include "pipe.h"
#include <stdarg.h>

typedef struct {
} PipeClassPartRec;

typedef struct _pipe_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;              /* the io part of the class information */
  FileClassPartRec filePart;
  PipeClassPartRec pipePart;
} PipeClassRec;

extern PipeClassRec PipeClass; /* the standard pointer to a pipe class record */

typedef struct _pipe_part_{
  int child;
  pipePo next,prev;                     /* pipes are connected to each other */
} PipePart;

typedef struct _pipe_object_ {
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
