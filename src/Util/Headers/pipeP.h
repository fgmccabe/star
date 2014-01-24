/* 
   Pipe library (private header)
   (c) 1994-2004 Imperial College and F.G. McCabe

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
  ManagedClassPartRec managedPart;
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
  ManagedRec managed;
  IoPart io;                            /* Io level of io object */
  FilePart file;                        /* File level of file object */
  PipePart pipe;                        /* Pipeet level part of the file */
} PipeObject;

#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_PIPE(c) ((pipePo)(checkCast((c),(classPo)&PipeClass)))
#else
#define O_PIPE(c) ((pipePo)(c))
#endif

#endif
