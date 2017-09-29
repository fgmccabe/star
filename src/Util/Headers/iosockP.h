/* 
   Socket library (private header)
   (c) 1994-2006 Imperial College and F.G. McCabe

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
   
   Contact: Francis McCabe <frankmccabe@mac.com>
*/ 

#ifndef _IO_SOCK_P_H_
#define _IO_SOCK_P_H_

#include "config.h"
#include "fileP.h"
#include "ioTcp.h"
#include <stdarg.h>

typedef struct {
  int dummy;
} SockClassPartRec;

typedef struct _sock_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;      /* The managed part of the sock */
  IoClassPartRec ioPart;              /* the io part of the class information */
  FileClassPartRec filePart;
  SockClassPartRec sockPart;
} SockClassRec;

extern SockClassRec SockClass; /* the standard pointer to a socket class record */

typedef struct _sock_part_{        /* The file specific part of a file object */
} SockPart;

typedef struct _sock_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockedRecord locked;                   /* The managed part of the socket */
  IoPart io;                            /* Io level of io object */
  FilePart file;                        /* File level of file object */
  SockPart sock;                        /* Socket level part of the file */
} SockObject;


#endif
