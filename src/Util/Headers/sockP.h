/* 
   Socket library (private header)
   (c) 1994-2006 Imperial College and F.G. McCabe

   Contact: Francis McCabe <frankmccabe@mac.com>
*/ 

#ifndef _IO_SOCK_P_H_
#define _IO_SOCK_P_H_

#include "config.h"
#include "fileP.h"
#include "sock.h"
#include <stdarg.h>

typedef struct {
  int dummy;
} SockClassPartRec;

typedef struct sock_class__ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;      /* The managed part of the sock */
  IoClassPartRec ioPart;              /* the io part of the class information */
  FileClassPartRec filePart;
  SockClassPartRec sockPart;
} SockClassRec;

extern SockClassRec SockClass; /* the standard pointer to a socket class record */

typedef struct sock_part__{        /* The file specific part of a file object */
} SockPart;

typedef struct sock_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockedRecord locked;                   /* The managed part of the socket */
  IoPart io;                            /* Io level of io object */
  FilePart file;                        /* File level of file object */
  SockPart sock;                        /* Socket level part of the file */
} SockObject;


#endif
