/* 
  File library (private header)
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
*/

#ifndef IO_FILE_P_H_
#define IO_FILE_P_H_

#include "ioP.h"
#include "file.h"
#include <aio.h>
#include <stdarg.h>
#include "signals.h"
#include "io.h"

typedef retCode (*fileProc)(filePo f);
typedef retCode (*seekProc)(filePo f, integer count);

typedef struct {
  seekProc seek;                        /* called when seeking */

  fileProc filler;                      // We use this to refill the buffer
  fileProc asyncFill;                   // We use this to refall asynchronously
  fileProc flush;                       // We use this to flush out the buffer
  fileProc asyncFlush;                  // We use this to flush asynchronously
} FileClassPartRec;

typedef struct file_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                // the io part of the class information */
  FileClassPartRec filePart;
} FileClassRec;

extern FileClassRec FileClass;
/* the standard pointer to a File class record */

typedef struct file_part_ {
  /* The file specific part of a file object */
  int fno;                              // The file number
  byte in_line[MAXLINE + 32];           // The input buffer */
  int16 in_pos;
  int16 in_len;
  integer file_pos;                     // Where are we in reading the file

  filePo prev;                            /* Previous file in open set */
  filePo next;                            /* Next file in open set */

  byte out_line[MAXLINE];               // The output buffer
  int16 out_pos;                        // Current position within the output buffer
  int16 wr_pos;                         // Where the last write call went to
  accessMode mode;                      // How is the file set up for reading/writing?
  completionSignaler signaler;
  void *signalerData;                   // Used as part of the callback
  struct aiocb aio;                     // Used during asynchronous IO
} FilePart;

typedef struct file_object_ {
  ObjectRec object;                     // object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            // Io level of io object */
  FilePart file;                        // File level of file object
} FileObject;

void initFileClass(classPo class, classPo request);
void inheritFileClass(classPo class, classPo request, classPo orig);
void fileInit(objectPo o, va_list *args);

retCode fileInputReady(ioPo io, integer count);
retCode fileOutputReady(ioPo io, integer count);
retCode fileInBytes(ioPo f, byte *ch, integer count, integer *actual);
retCode fileOutBytes(ioPo f, byte *b, integer count, integer *actual);
retCode fileBackByte(ioPo f, byte b);
retCode fileAtEof(ioPo f);

retCode flSeek(filePo f, integer pos);
retCode fileClose(ioPo io);
retCode refillBuffer(filePo f);
retCode fileFill(filePo f);
retCode fileFlush(filePo f);

#endif
