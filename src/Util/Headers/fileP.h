/* 
   File library (private header)
   (c) 1994-2000 Imperial College and F.G. McCabe

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

#ifndef _IO_FILE_P_H_
#define _IO_FILE_P_H_

#include "ioP.h"
#include "file.h"
#include <stdarg.h>

typedef retCode (*fileConfProc)(filePo f,ioConfigOpt mode);
typedef retCode (*fileProc)(filePo f);

typedef struct {
  fileConfProc configure;               /* We use this to configure files */
  fileProc filler;                      /* We use this to refill the buffer */
} FileClassPartRec;

typedef struct _file_class_ {
  ObjectClassRec objectPart;
  ManagedClassPartRec managedPart;
  IoClassPartRec ioPart;              /* the io part of the class information */
  FileClassPartRec filePart;
} FileClassRec;

extern FileClassRec FileClass;  /* the standard pointer to an File class record */

typedef struct _file_part_{	   /* The file specific part of a file object */
  int fno;                              /* The file number */
  byte in_line[MAXLINE+32];             /* The input buffer */
  short in_pos;
  short in_len;

  byte out_line[MAXLINE];               /* The output buffer */
  short out_pos;                        /* Current position within the output buffer */

  ioEncoding encoding;                  /* current encoding in the file */
  charOutProc charOut;                  /* Encoding character function */
  charInProc charIn;                    /* Character decoding function */
} FilePart;

typedef struct _file_object_ {
  ObjectRec object;                     /* object level of the io structure */
  ManagedRec managed;                   /* The managed part of the socket */
  IoPart io;                            /* Io level of io object */
  FilePart file;                        /* File level of file object */
} FileObject;

void stopAlarm(void);       /* stop the cpu timer from delivering any signals */
void startAlarm(void);                  /* enable the virtual timer again */


#endif
