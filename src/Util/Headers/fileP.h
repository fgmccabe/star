/* 
  File library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe
*/

#ifndef _IO_FILE_P_H_
#define _IO_FILE_P_H_

#include "ioP.h"
#include "file.h"
#include <aio.h>
#include <stdarg.h>

typedef retCode (*fileConfProc)(filePo f, ioConfigOpt mode);
typedef retCode (*fileProc)(filePo f);
typedef retCode (*seekProc)(filePo f, integer count);
typedef logical (*statusProc)(filePo f);

typedef struct {
  fileConfProc configure;               // We use this to configure files */
  seekProc seek;                        /* called when seeking */

  statusProc inReady;                   /* Called to determine if file has input */
  statusProc outReady;                  /* Called to determine if file can output */

  fileProc filler;                      // We use this to refill the buffer
} FileClassPartRec;

typedef struct file_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                // the io part of the class information */
  FileClassPartRec filePart;
} FileClassRec;

FileClassRec FileClass;
/* the standard pointer to an File class record */

typedef struct file_part_ {
  /* The file specific part of a file object */
  int fno;                              // The file number
  byte in_line[MAXLINE + 32];           // The input buffer */
  int16 in_pos;
  int16 in_len;

  long bufferPos;                       // Mark at beginning of this buffer

  byte out_line[MAXLINE];               // The output buffer
  int16 out_pos;                        // Current position within the output buffer
  fileCallBackProc ioReadyProc;         // Called when i/o is ready (if configured)
  void *ioReadClientData;               // Used as part of the callback
  struct aiocb aio;                     // Used during asynchronous IO
} FilePart;

typedef struct file_object_ {
  ObjectRec object;                     // object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            // Io level of io object */
  FilePart file;                        // File level of file object
} FileObject;

void initFileClass(classPo class, classPo request);
void FileInit(objectPo o, va_list *args);

retCode fileInBytes(ioPo f, byte *ch, integer count, integer *actual);
retCode fileOutBytes(ioPo f, byte *b, integer count, integer *actual);
retCode fileBackByte(ioPo f, byte b);
retCode fileAtEof(ioPo f);
retCode fileEnqueueRead(ioPo f, integer count, void *cl);
retCode fileEnqueueWrite(ioPo f, byte *buffer, integer count, void *cl);

logical fileInReady(filePo f);
logical fileOutReady(filePo f);

retCode fileFlusher(ioPo f, long count);
retCode flSeek(filePo f, integer count);
retCode fileClose(ioPo f);
retCode refillBuffer(filePo f);
retCode fileFill(filePo f);
retCode fileFlush(filePo f, long count);
retCode fileConfigure(filePo, ioConfigOpt mode);

#endif
