/* 
  File library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _IO_FILE_P_H_
#define _IO_FILE_P_H_

#include "ioP.h"
#include "file.h"
#include <stdarg.h>

typedef retCode (*fileConfProc)(filePo f, ioConfigOpt mode);
typedef retCode (*fileProc)(filePo f);

typedef struct {
  fileConfProc configure;               // We use this to configure files */
  fileProc filler;                      // We use this to refill the buffer
} FileClassPartRec;

typedef struct _file_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                // the io part of the class information */
  FileClassPartRec filePart;
} FileClassRec;

extern FileClassRec FileClass;
/* the standard pointer to an File class record */

typedef struct _file_part_ {
  /* The file specific part of a file object */
  int fno;                              // The file number
  byte in_line[MAXLINE + 32];           // The input buffer */
  int16 in_pos;
  int16 in_len;

  long bufferPos;                       // Mark at beginning of this buffer

  byte out_line[MAXLINE];               // The output buffer
  int16 out_pos;                        // Current position within the output buffer
} FilePart;

typedef struct _file_object_ {
  ObjectRec object;                     // object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            // Io level of io object */
  FilePart file;                        // File level of file object
} FileObject;

void stopAlarm(void);                   // stop the cpu timer from delivering any signals
void startAlarm(void);                  // enable the virtual timer again

void inheritFile(classPo class, classPo request);
void initFileClass(classPo class, classPo request);
void FileInit(objectPo o, va_list *args);

retCode fileInBytes(ioPo f, byte *ch, integer count, integer *actual);
retCode fileOutBytes(ioPo f, byte *b, integer count, integer *actual);
retCode fileBackByte(ioPo f, byte b);
retCode fileMark(ioPo f, integer *mark);
retCode fileReset(ioPo f, integer mark);
retCode fileAtEof(ioPo f);
retCode fileInReady(ioPo f);
retCode fileOutReady(ioPo f);

retCode fileFlusher(ioPo f, long count);
retCode fileSeek(ioPo f, integer count);
retCode fileClose(ioPo f);
retCode refillBuffer(filePo f);
retCode fileFill(filePo f);
retCode fileFlush(filePo f, long count);
retCode fileConfigure(filePo, ioConfigOpt mode);

#endif
