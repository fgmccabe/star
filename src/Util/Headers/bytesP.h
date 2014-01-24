/* 
   Byte string library (private header)
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

#ifndef _IO_BYTE_P_H_
#define _IO_BYTE_P_H_

#include "ioP.h"
#include "bytes.h"
#include <stdarg.h>

typedef struct {
} ByteClassPartRec;

typedef struct _byte_class_ {
  ObjectClassRec objectPart;
  ManagedClassPartRec managedPart;
  IoClassPartRec ioPart;              /* the io part of the class information */
  ByteClassPartRec bytePart;
} ByteClassRec;

typedef struct _byte_part_{        /* The byte specific part of a byte object */
  byte *buffer;                         /* The data buffer */
  long pos;
  long len;
  logical resizeable;                   /* Is this byte object resizeable? */
  ioEncoding encoding;                  /* What encoding is in force? */
  charOutProc charOut;                  /* Encoding character function */
  charInProc charIn;                    /* Character decoding function */
} BytePart;

typedef struct _byte_object_ {
  ObjectRec object;                     /* object level of the io structure */
  ManagedRec managed;
  IoPart io;                            /* Io level of io object */
  BytePart byte;                        /* Byte level of byte string object */
} ByteObject;

#endif
