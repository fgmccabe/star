/* 
  File level access to I/O
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _IO_FILE_H_
#define _IO_FILE_H_

#include "config.h"
#include "io.h"
#include "unistr.h"

extern ioPo stdIn;    /* Standard input  */
extern ioPo stdOut;    /* Standard output */
extern ioPo stdErr;    /* Standard error */

typedef enum {
  turnOffBlocking, turnOnBlocking, enableAsynch, disableAsynch
} ioConfigOpt;

typedef struct file_object_ *filePo;

extern classPo fileClass;

ioPo openInFile(char *file, ioEncoding encoding);
ioPo openOutFile(char *file, ioEncoding encoding);
ioPo openInOutFile(char *file, ioEncoding encoding);
ioPo openAppendFile(char *file, ioEncoding encoding);
ioPo openInOutAppendFile(char *file, ioEncoding encoding);
ioPo newOutFile(char *file, ioEncoding encoding);

retCode filePresent(char *name);
retCode isRegularFile(char *fname);
retCode isDirectory(char *fname);
logical isExecutableFile(char *file);

integer inText(filePo f, char *buffer, integer len);

ioPo Stdin(void);
ioPo Stdout(void);
ioPo Stderr(void);

int fileNumber(filePo f);
ioEncoding fileEncoding(filePo f);

retCode configureIo(filePo f, ioConfigOpt mode);
logical isFileBlocking(filePo f);
logical isFileAsynch(filePo f);

retCode fileSeek(filePo f, integer pos);

void setup_stdin(void);
void reset_stdin(void);
retCode initLogfile(char *name);

logical isInReady(filePo f);
logical isOutReady(filePo f);

retCode skipShellPreamble(filePo f);
extern ioPo logFile;    /* The standard place to write logging msgs */

extern char *
resolveFileName(char *base, const char *path, integer pathLen, char *buff, integer buffLen);
extern retCode resolvePath(char *root, integer rootLen, const char *fn, integer fnLen, char *buff, integer buffLen);

#ifdef VERIFY_OBJECT
#define O_FILE(c) ((filePo)(checkCast((c),ioClass)))
#else
#define O_FILE(c) ((filePo)(c))
#endif

#endif
