/* 
   File level access to I/O
   (c) 1994-2010 F.G. McCabe

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
   
   Contact: Francis McCabe <fmccabe@gmail.com>
*/ 

#ifndef _IO_FILE_H_
#define _IO_FILE_H_

#include "config.h"
#include "io.h"

extern ioPo stdIn;		/* Standard input  */
extern ioPo stdOut;		/* Standard output */
extern ioPo stdErr;		/* Standard error */

typedef enum {turnOffBlocking, turnOnBlocking, enableAsynch,disableAsynch
             }ioConfigOpt;

typedef struct _file_object_ *filePo;

extern classPo fileClass;

typedef enum { regularFileType, dirFileType, 
	       linkFileType, socketFileType, unknownFileType } fileType;

fileType typeOfFile(uniChar *name);
logical filePresent(uniChar *name);
retCode regularFile(uniChar *name,char *fname,long len);
retCode isDirectory(uniChar *name);
retCode isDirectoryFile(uniChar *name,char *fname,long len);

typedef retCode (*dirProc)(uniChar *name,fileType type,void *cl);
retCode processDirectory(uniChar *name,dirProc proc,void *cl);

ioPo openInFile(uniChar *file,ioEncoding encoding);
ioPo openOutFile(uniChar *file,ioEncoding encoding);
ioPo openInOutFile(uniChar *file,ioEncoding encoding);
ioPo openAppendFile(uniChar *file,ioEncoding encoding);
ioPo openInOutAppendFile(uniChar *file,ioEncoding encoding);
ioPo newOutFile(uniChar *file,ioEncoding encoding);

uniChar inCh(ioPo f);                 /* read character from a file */

ioPo OpenStdin(void);
ioPo OpenStdout(void);
ioPo OpenStderr(void);

int fileNumber(filePo f);
ioEncoding fileEncoding(filePo f);

retCode openPipe(char *exec,char **argv,char **envv,ioPo *inpipe,ioPo *outpipe,ioPo *errPipe,
                 ioEncoding encoding);
retCode rmFile(uniChar *name);
retCode mvFile(uniChar *name,uniChar *to);

void setEncoding(filePo f,ioEncoding encoding);
retCode configureIo(filePo f,ioConfigOpt mode);

void setup_stdin(void);
void reset_stdin(void);
retCode initLogfile(uniChar *name);
extern ioPo logFile;		/* The standard place to write logging msgs */

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_FILE(c) ((filePo)(checkCast((c),fileClass)))
#else
#define O_FILE(c) ((filePo)(c))
#endif


#endif
