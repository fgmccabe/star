/* 
  File level access to I/O
  Copyright (c) 2016, 2017, 2024 and beyond Francis G. McCabe
*/

#ifndef _IO_FILE_H_
#define _IO_FILE_H_

#include <aio.h>
#include <stdarg.h>
#include "config.h"
#include "unistr.h"
#include "ioP.h"
#include "signals.h"

typedef enum {
  blocking, asynch
} accessMode;

typedef struct file_object_ *filePo;

extern classPo fileClass;

void closeAllFiles(void);                     /* Close down the file system */

ioPo openInFile(char *file, ioEncoding encoding);
ioPo openOutFile(char *file, ioEncoding encoding);
ioPo openAppendFile(char *file, ioEncoding encoding);
ioPo openInOutAppendFile(char *file, ioEncoding encoding);
ioPo newOutFile(char *file, ioEncoding encoding);

retCode filePresent(char *name);
retCode isRegularFile(char *fname);
retCode isDirectory(char *fname);
logical isExecutableFile(char *file);

integer inText(filePo f, char *buffer, integer len);

retCode waitForAsync(filePo files[],integer len, integer timeout);

logical isInAsync(filePo f);

retCode flushFile(filePo f);            /* file flush */
retCode flushIo(ioPo io);               // Version for io channels
void flushOut(void);                    /* flush all files */

ioPo Stdin(void);
ioPo Stdout(void);
ioPo Stderr(void);

int fileNumber(filePo f);

logical isFileBlocking(filePo f);
logical isFileAsynch(filePo f);

accessMode fileAccessMode(filePo f);
void resetAccessMode(filePo f, accessMode mode);

retCode enableASynch(filePo f);
retCode disableASynch(filePo f);

typedef void (*completionSignaler)(filePo f, void *cl);

retCode enqueueRead(filePo f, completionSignaler signaler, void *cl);
retCode enqueueWrite(filePo f, completionSignaler signaler, void *cl);
progress asyncRdStatus(filePo f);
progress asyncWrStatus(filePo f);

void setup_stdin(void);
void reset_stdin(void);
retCode initLogfile(char *name);

retCode skipShellPreamble(filePo f);

char *resolveFileName(char *base, const char *path, integer pathLen, char *buff, integer buffLen);

#ifdef VERIFY_OBJECT
#define O_FILE(c) ((filePo)(checkCast((c),fileClass)))
#else
#define O_FILE(c) ((filePo)(c))
#endif

logical isAFile(objectPo o);

#endif
