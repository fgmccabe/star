/*
  File I/O class
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"    // Invoke configuration header
#include "fileP.h"

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <formio.h>

#ifdef HAVE_RLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef SYSV
#include <stropts.h>
#endif

#ifndef STD_PERMISSIONS
#define STD_PERMISSIONS (S_IRUSR|S_IRGRP|S_IROTH|S_IWUSR)
#endif

/* Set up the file class */

FileClassRec FileClass = {
  {
    (classPo) &IoClass,                   // parent class is io object
    "file",                               // this is the file class
    inheritFile,                          // file inheritance
    initFileClass,                        // File class initializer, phase I
    O_INHERIT_DEF,                        // File object element creation
    NULL,                                 // File objectdestruction
    O_INHERIT_DEF,                        // erasure
    FileInit,                             // initialization of a file object
    sizeof(FileObject),                   // size of a file object
    O_INHERIT_DEF,                        // Hashcode for files
    O_INHERIT_DEF,                        // Equality for files
    NULL,                                 // pool of file values
    PTHREAD_ONCE_INIT,                    // not yet initialized
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    fileInBytes,                        // inByte
    fileOutBytes,                       // outBytes
    fileBackByte,                       // put a byte back in the buffer
    fileMark,                           // Mark the file
    fileReset,                          // Reset to mark
    fileAtEof,                          // Are we at end of file?
    fileInReady,                        // readyIn
    fileOutReady,                       // readyOut
    fileFlusher,                        // flush
    fileSeek,                           // seek
    fileClose                           // close
  },
  {
    fileConfigure,                       // configure a file
    fileFill                             // refill the buffer if needed
  }
};

classPo fileClass = (classPo) &FileClass;

// Implement inheritance of file specific portions of class

void inheritFile(classPo class, classPo request) {
  FileClassRec *req = (FileClassRec *) request;
  FileClassRec *template = (FileClassRec *) class;

  logical done = False;

  while (!done) {
    done = True;

    if (req->filePart.configure == O_INHERIT_DEF) {
      if (template->filePart.configure != O_INHERIT_DEF)
        req->filePart.configure = template->filePart.configure;
      else
        done = False;
    }

    if (req->filePart.filler == O_INHERIT_DEF) {
      if (template->filePart.filler != O_INHERIT_DEF)
        req->filePart.filler = template->filePart.filler;
      else
        done = False;
    }
    template = (FileClassRec *) (template->objectPart.parent);
  }
}

void initFileClass(classPo class, classPo request) {
  assert(request->pool != NULL);
}

// IO initialization should already be done at this point
void FileInit(objectPo o, va_list *args) {
  filePo f = O_FILE(o);

  // Set up the buffer pointers
  f->file.in_pos = 0;
  f->file.out_pos = 0;
  f->file.in_len = 0;
  f->file.fno = va_arg(*args, int);             // set up the file number
  configureIo(f, turnOnBlocking);
  setEncoding(O_IO(f), va_arg(*args, ioEncoding));     // set up the encoding
  f->io.mode = va_arg(*args, ioDirection);
}

// Implement class file functions


// convenience function for reading characters
codePoint inCh(ioPo f) {
  codePoint ch;

  switch (inChar(f, &ch)) {
    case Ok:
      return ch;
    case Eof:
      return uniEOF;
    case Interrupt:
    case Fail: {
      filePo fl = O_FILE(f);
      configureIo(fl, turnOnBlocking);

      again:
      switch (inChar(f, &ch)) {
        case Ok:
          break;
        case Fail:
        case Interrupt:
          goto again;                       // pretty busy loop
        default:
          return ioErrorMsg(f, "issue in reading char");
      }

      configureIo(fl, turnOffBlocking);
      return ch;
    }
    case Error:
      return Error;
    default:
      return Error;
  }
}

retCode fileBackByte(ioPo io, byte b) {
  filePo f = O_FILE(io);

  if (f->file.in_pos < 1) {
    if (f->file.in_len - f->file.in_pos < NumberOf(f->file.in_line)) {
      memmove(&f->file.in_line[1], &f->file.in_line[f->file.in_pos],
              sizeof(byte) * (f->file.in_len - f->file.in_pos));
      f->file.in_line[0] = b;
      f->file.in_len = (int16) (f->file.in_len - f->file.in_pos + 1);
      f->file.in_pos = 0;
      f->io.status = Ok;
    } else
      return Error;
  } else {
    f->file.in_pos--;
    f->file.in_line[f->file.in_pos] = b;
    f->io.status = Ok;
  }

  return Ok;
}

retCode fileInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  filePo f = O_FILE(io);

  while (ret == Ok && remaining > 0) {
    if (f->file.in_pos >= f->file.in_len)
      ret = refillBuffer(f);
    if (ret == Ok) {
      *ch++ = f->file.in_line[f->file.in_pos++];
      remaining--;
    }
  }
  *actual = count - remaining;
  if (*actual > 0 && ret != Error)
    return Ok;
  else
    return ret;
}

retCode fileOutBytes(ioPo io, byte *b, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  filePo f = O_FILE(io);

  while (ret == Ok && remaining > 0) {
    if (f->file.out_pos >= NumberOf(f->file.out_line))
      ret = fileFlush(f, 0);
    if (ret == Ok && f->file.out_pos < NumberOf(f->file.out_line)) {
      f->file.out_line[f->file.out_pos++] = *b++;
      remaining--;
    }
  }
  *actual = count - remaining;
  return ret;
}

retCode fileAtEof(ioPo io) {
  filePo f = O_FILE(io);

  if (f->file.in_pos < f->file.in_len)
    return Ok;
  else
    return refillBuffer(f);
}

retCode fileInReady(ioPo io) {
  filePo f = O_FILE(io);

  if (f->file.in_pos < f->file.in_len)
    return Ok;
  else {
    int fno = f->file.fno;

    if ((f->io.mode & ioREAD) != 0) {
      fd_set fdin;
      struct timeval period;

      FD_ZERO(&fdin);

      FD_SET(fno, &fdin);

      period.tv_sec = 0;
      period.tv_usec = 0;

      if (select(fno + 1, &fdin, NULL, NULL, &period) > 0)
        return Ok;
      else
        return Fail;
    } else
      return ioErrorMsg(io, "%s does not permit read access", fileName(io));
  }
}

retCode fileOutReady(ioPo io) {
  filePo f = O_FILE(io);

  if (f->file.out_pos < NumberOf(f->file.out_line))
    return Ok;
  else {
    int fno = f->file.fno;

    if ((f->io.mode & ioWRITE) != 0) {
      fd_set fdout;
      struct timeval period;

      FD_ZERO(&fdout);

      FD_SET(fno, &fdout);

      period.tv_sec = 0;
      period.tv_usec = 0;

      if (select(fno + 1, NULL, &fdout, NULL, &period) > 0)
        return Ok;
      else
        return Fail;
    } else
      return ioErrorMsg(io, "%s does not permit write access", fileName(io));
  }
}

retCode fileFlusher(ioPo io, long count) {
  filePo f = O_FILE(io);

  return fileFlush(f, count);
}

retCode fileSeek(ioPo io, integer count) {
  filePo f = O_FILE(io);

  flushFile(io);

  if (lseek(f->file.fno, count, SEEK_SET) == -1)
    return ioErrorMsg(io, "problem %s (%d) in positioning %s", strerror(errno), errno,
                      fileName(O_IO(f)));
  else {
    f->file.in_pos = f->file.out_pos = 0;
    f->file.in_len = 0;                 // ensure that we will be refilling

    if (isReadingFile(io) == Ok) {
      f->io.inCpos = count;
      f->io.inBpos = count;
    }
    if (isWritingFile(io)) {
      f->io.outBpos = count;
    }
    f->io.status = Ok;
    return Ok;
  }
}

retCode fileClose(ioPo io) {
  filePo f = O_FILE(io);
  retCode ret = configureIo(f, turnOnBlocking);
  objectPo o = O_OBJECT(io);

  lock(O_LOCKED(o));

  while ((ret = flushFile(io)) == Fail);

  if (ret == Ok) {
    if (f->file.fno >= 0) {
      if (close(f->file.fno) < 0) {
        switch (errno) {
          case EINTR:
            ret = Interrupt;
            break;
          default:
            ret = ioErrorMsg(io, "problem %s (%d) in closing %s", strerror(errno), errno, fileName(io));
            break;
        }
      } else {
        f->file.fno = -1;
        ret = Ok;
      }
    } else
      ret = Ok;        // probably already being closed
  }

  unlock(O_LOCKED(o));
  destroyObject(O_OBJECT(o)); // this will get rid of all the file objects attributes
  return ret;
}

/* Regular file functions */

ioPo logFile = NULL;
/* File to write the log to */
ioPo stdIn = NULL;
ioPo stdOut = NULL;
ioPo stdErr = NULL;

static sigset_t blocked;

void stopAlarm(void)    // stop the cpu timer from delivering alarm signals
{
  sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, SIGVTALRM);  // The CPU timer

  sigprocmask(SIG_BLOCK, &set, &blocked);
}

void startAlarm(void)                   // enable the virtual timer again
{
  sigprocmask(SIG_SETMASK, &blocked, NULL);
}

/* Standard functions for re-filling the file buffer, flushing it out and
   closing the file */

retCode refillBuffer(filePo f) {
  return ((FileClassRec *) (f->object.class))->filePart.filler(f);
}

retCode fileFill(filePo f) {
  if (f->file.in_pos >= f->file.in_len) {  // nead to read more input?
    ssize_t len;
    int lerrno;                         // local copy of errno

    stopAlarm();      // Stop the time interrupt
    len = read(f->file.fno, f->file.in_line, (size_t) MAXLINE);
    lerrno = errno;

    startAlarm();      // Restart the timer interrupt

    if (len < 0) {        // something wrong?
      switch (lerrno) {
        case EINTR:      // call was interrupted
          f->io.status = Interrupt;
          // logMsg(logFile,"IO (%d) was interrupted",f->file.fno);
          return Interrupt;
        case EAGAIN:      // Not ready
          f->io.status = Fail;
          // logMsg(logFile,"IO (%d) was not ready",f->file.fno);
          return Fail;
        default:        // Pretend its uniEOF
          // logMsg(logFile,"IO issue at fd %d: (%s) %d",f->file.fno,
          //strerror(lerrno),lerrno);

          f->file.in_pos = f->file.in_len = 0;
          return f->io.status = Eof;  // we have reach end of file
      }
    } else {
      f->file.in_pos = 0;
      f->file.in_len = (int16) len;
      f->file.bufferPos = f->io.inBpos;

      if (len == 0) {
        //	logMsg(logFile,"IO (%d) ended",f->file.fno);
        return f->io.status = Eof;
      } else {
        // logMsg(logFile,"read %d bytes from %d",len,f->file.fno);
        return f->io.status = Ok;
      }
    }
  } else
    return Ok;        // Already got stuff in there
}

retCode fileFlush(filePo f, long count) {
  int fno = f->file.fno;
  long written;
  int16 remaining = f->file.out_pos;
  byte *cp = f->file.out_line;
  long writeGap = 0;

  if (count > 0 && f->file.out_pos + count < NumberOf(f->file.out_line))
    return Ok;

  while (remaining > 0 && (written = write(fno, cp, (size_t) remaining)) != remaining) {
    if (written == -1) {
      switch (errno) {
        case 0:                           // Linux and/or solaris sometimes does this
          continue;
        case EINTR:
          if (writeGap > 0) {
            memmove(&f->file.out_line[0], cp, sizeof(byte) * remaining);
            f->file.out_pos = remaining;
          }
          return Interrupt;    // report an interrupt
        case EAGAIN:        // we shuffle the remaining buffer to the front
          if (writeGap > 0) {
            memmove(&f->file.out_line[0], cp, sizeof(byte) * remaining);
            f->file.out_pos = remaining;
          }
          return Fail;
        default:
          return ioErrorMsg(O_IO(f),
                            "Problem %s (%d) in writing to %s", strerror(errno), errno,
                            fileName(O_IO(f)));
      }
    } else {
      cp += written;
      writeGap += written;
      remaining -= written;
    }
  }
  f->file.out_pos = 0;
  f->io.status = Ok;
  return Ok;
}

retCode fileMark(ioPo f, integer *mark) {
  if ((fileMode(f) & ioREAD) != ioREAD)
    return Error;
  else {
    *mark = f->io.inBpos;
    return Ok;
  }
}

retCode fileReset(ioPo f, integer mark) {
  if ((fileMode(f) & ioREAD) != ioREAD)
    return Error;
  else {
    filePo ff = O_FILE(f);
    if (mark < ff->file.bufferPos || mark > ff->file.in_len)
      return Fail;
    else {
      ff->io.inBpos = mark;
      ff->file.in_pos = (int16) (mark - ff->file.bufferPos);
      return Ok;
    }
  }
}

ioEncoding fileEncoding(filePo f) {
  return f->io.encoding;
}

logical isFileBlocking(filePo f) {
  int fno = f->file.fno;
  long flag = fcntl(fno, F_GETFL);

  return flag & O_NONBLOCK ? False : True;
}

logical isFileAsynch(filePo f) {
  int fno = f->file.fno;
  long flag = fcntl(fno, F_GETFL);

  return flag & O_ASYNC ? True : False;
}

retCode configureIo(filePo f, ioConfigOpt mode) {
  return ((FileClassRec *) f->object.class)->filePart.configure(f, mode);
}

retCode fileConfigure(filePo f, ioConfigOpt mode) {
  int fno = f->file.fno;
  long flag = fcntl(fno, F_GETFL);

  switch (mode) {
    case turnOffBlocking:
      flag |= O_NONBLOCK;    // make file non-blocking
      break;
    case turnOnBlocking:
      flag &= ~O_NONBLOCK;  // make file blocking again
      break;

    case enableAsynch: {    // Enable asynchronous I/O
#ifdef SYSV
      ioState mode = fileMode(f);
      if(ioctl(fno,I_SETSIG,((mode&ioREAD)?S_INPUT:0)|
         ((mode&ioWRITE)?S_OUTPUT:0))>=0)
        return Ok;
      else if(errno==EINVAL)	// not an error, just a failure
        return Fail;
      else
        return ioErrorMsg(O_IO(f),"problem %s (%d) in configuring %s",strerror(errno),errno,fileName(f));
#else
      if (fno > 2 && fcntl(fno, F_SETOWN, getpid()) < 0)
        logMsg(logFile, "Warning: couldnt set ownership of fd %d", fno);
      flag |= O_ASYNC;
#endif
      break;
    }
    case disableAsynch:
#ifdef SYSV
      if(ioctl(fno,I_SETSIG,0)>=0)
        return Ok;
      else
        return ioErrorMsg(O_IO(f),
        "problem %s (%d) in configuring %s",strerror(errno),errno,
                          fileName(O_IO(f)));
#else
      flag &= ~O_ASYNC;  // We no longer want to receive interrupts
#endif
      break;
    default:      // nothing
      ;
  }

  if (fcntl(fno, F_SETFL, flag) == -1)
    return ioErrorMsg(O_IO(f), "problem %s (%d) in configuring %s",
                      strerror(errno), errno, fileName(O_IO(f)));
  else
    return Ok;
}

int fileNumber(filePo f) {
  return f->file.fno;
}

/* Open a file for input only */
ioPo openInFile(char *name, ioEncoding encoding) {
  if (isRegularFile(name) == Ok) {
    int inFileRefNum = open((const char *) name, O_RDONLY);

    if (inFileRefNum == -1)
      return NULL;
    else
      return O_IO(newObject(fileClass, name, inFileRefNum, encoding, ioREAD));
  } else
    return NULL;
}

ioPo openOutFile(char *name, ioEncoding encoding) {
  switch (isRegularFile(name)) {
    case Ok:
    case Fail: {      // File not previously found
      int outFileRefNum = open((const char *) name, O_WRONLY | O_CREAT | O_TRUNC, STD_PERMISSIONS);

      if (outFileRefNum == -1)
        return NULL;
      else
        return O_IO(newObject(fileClass, name, outFileRefNum, encoding, ioWRITE));
    }
    default:
      return NULL;
  }
}

/* Open a file for input and output */
ioPo openInOutFile(char *name, ioEncoding encoding) {
  switch (isRegularFile(name)) {
    case Ok:
    case Fail: {      // File not previously found
      int outFileRefNum = open((const char *) name, O_RDWR | O_CREAT, STD_PERMISSIONS);

      if (outFileRefNum == -1)
        return NULL;
      else
        return O_IO(newObject(fileClass, name, outFileRefNum, encoding, ioREAD | ioWRITE));
    }
    default:
      return NULL;
  }
}

/* create and/or truncate existing file */
ioPo newOutFile(char *name, ioEncoding encoding) {
  switch (isRegularFile(name)) {
    case Ok:
    case Fail: {      // File not previously found
      int outFileRefNum = open((const char *) name, O_WRONLY | O_TRUNC | O_CREAT, STD_PERMISSIONS);

      if (outFileRefNum == -1)
        return NULL;
      else
        return O_IO(newObject(fileClass, name, outFileRefNum, encoding, ioWRITE));
    }
    default:
      return NULL;
  }
}

ioPo openAppendFile(char *name, ioEncoding encoding) {
  switch (isRegularFile(name)) {
    case Ok:
    case Fail: {      // File not previously found
      int outFileRefNum = open((const char *) name, O_WRONLY | O_CREAT | O_APPEND, STD_PERMISSIONS);

      if (outFileRefNum == -1)
        return NULL;
      else
        return O_IO(newObject(fileClass, name, outFileRefNum, encoding, ioWRITE));
    }
    default:
      return NULL;
  }
}

ioPo openInOutAppendFile(char *name, ioEncoding encoding) {
  switch (isRegularFile(name)) {
    case Ok:
    case Fail: {      // File not previously found
      int outFileRefNum = open((const char *) name, O_RDWR | O_APPEND | O_CREAT, STD_PERMISSIONS);

      if (outFileRefNum == -1)
        return NULL;
      else
        return O_IO(newObject(fileClass, name, outFileRefNum, encoding, ioREAD | ioWRITE));
    }
    default:
      return NULL;
  }
}

retCode isRegularFile(char *fname) {
  struct stat buf;

  if (stat((const char *) fname, &buf) == -1)
    return Fail;    /* File not found */
  else if (S_ISDIR(buf.st_mode))
    return Fail;
  else
    return Ok;
}

retCode isDirectory(char *fname) {
  struct stat buf;

  if (stat((const char *) fname, &buf) == -1)
    return Fail;    /* File not found */
  else if (S_ISDIR(buf.st_mode))
    return Ok;
  else
    return Fail;
}

/* return True if a file is really an executable program or not.
   Not checked out for windows at this time
   */
logical isExecutableFile(char *file) {
  uid_t id = geteuid();
  gid_t grp = getegid();

  struct stat buf;

  if (stat(file, &buf) == -1 || !S_ISREG(buf.st_mode))
    return False;

  if (buf.st_mode & S_IXOTH)  /* anyone can execute this file */
    return True;
  if (buf.st_mode & S_IXGRP) {
    if (buf.st_gid == grp)
      return True;
    else {
      int ngroups = getgroups(0, NULL); /* The number of supplementary groups */

      if (ngroups > 0) {
        gid_t *groups = (gid_t *) calloc((size_t) ngroups, sizeof(gid_t));
        int i;

        getgroups(ngroups, groups);

        for (i = 0; i < ngroups; i++)
          if (groups[i] == buf.st_gid) {
            free(groups);
            return True;
          }

        free(groups);
      }
    }
  }
  if (buf.st_mode & S_IXUSR)
    return (logical) (id == buf.st_uid);
  return False;
}

/* Special macro for Windows 95 */
#define FILE_ACCESS_MODE F_OK|R_OK

/* Check if a file is present or not */
retCode filePresent(char *name) {
  if (access((const char *) name, FILE_ACCESS_MODE) == 0)
    return Ok;
  else
    return Fail;
}

/* These only apply to Unix */

/* We pipe our standard through the forked process, so that we may set
   our standard input non-blocking without affecting any other
   processes that may expect the original descriptor to be blocking
   (eg. more) */

#ifndef ALLTRACE
static void bufferStdin(int outfd)
{
  fd_set rfds;
  char data[4096];
  int len;
  
  FD_SET(0,&rfds);
  while(select(1,&rfds,NULL,NULL,NULL) > 0 &&
  (len = read(0,data,4096)) > 0) {
    write(outfd,data,len);
    FD_SET(0,&rfds);
  }
}

static int stdInProc = 0;

static void killStdIn(void)
{
  kill(stdInProc,SIGTERM);
}

#ifndef ALLTRACE_
static int getStdinPipe()
{
  static int fds[2] = {0, 0};

  if(pipe(fds) == 1) {
    logMsg(logFile,"Warning: failed to create pipe for stdin");
    return fds[0];
  }

  switch(stdInProc=fork()) {
  case 0: // Child process
    bufferStdin(fds[1]);
    exit(0);

  case -1: // Fork Failed
    logMsg(logFile,"Warning: failed to fork child process for stdin");
    break;

  default: // Parent process
    atexit(killStdIn);		// ugh!
  }
  
  return fds[0];
}
#endif
#endif

ioPo Stdin(void) {
  if (stdIn == NULL) {
#if 0
    //#ifndef ALLTRACE_
    int fd = getStdinPipe();
#else
    int fd = 0;
#endif
    byte stdinName[] = {'s', 't', 'd', 'i', 'n', 0};

    stdIn = O_IO(newObject(fileClass, stdinName, fd, utf8Encoding, ioREAD));
    configureIo(O_FILE(stdIn), turnOnBlocking);
  }
  return stdIn;
}

ioPo Stdout(void) {
  if (stdOut == NULL) {
    byte stdoutName[] = {'s', 't', 'd', 'o', 'u', 't', 0};

    stdOut = O_IO(newObject(fileClass, stdoutName, 1, utf8Encoding, ioWRITE));
  }
  return stdOut;
}

ioPo Stderr(void) {
  if (stdErr == NULL) {
    byte stderrName[] = {'s', 't', 'd', 'e', 'r', 'r', 0};

    stdErr = O_IO(newObject(fileClass, stderrName, 2, utf8Encoding, ioWRITE));
  }
  return stdErr;
}

retCode initLogfile(char *name) {
  if (uniIsLit(name, "-"))
    logFile = Stderr();
  else
    logFile = openAppendFile(name, utf8Encoding);
  if (logFile != NULL) {
    Stdin();    // open standard files
    Stdout();
    return Ok;
  } else
    return Error;
}

/* Some special functions to set up and reset the standard input for
   non-blocking operation */
void setup_stdin(void) {
  configureIo(O_FILE(stdIn), turnOffBlocking);
  configureIo(O_FILE(stdIn), enableAsynch);
}

void reset_stdin(void) {
  if (stdIn != NULL) {
    configureIo(O_FILE(stdIn), turnOnBlocking);
    configureIo(O_FILE(stdIn), disableAsynch);
  }
}

retCode rewindFile(filePo f) {
  if (!isSubClass(classOfObject(O_OBJECT(f)), fileClass))
    return ioErrorMsg(O_IO(f), "%s is not a regular file", fileName(O_IO(f)));

  flushFile(O_IO(f));

  if (lseek(f->file.fno, 0, 0) == -1)
    return ioErrorMsg(O_IO(f), "problem %s (%d) in rewinding %s", strerror(errno), errno,
                      fileName(O_IO(f)));
  else {
    f->file.in_pos = f->file.out_pos = 0;
    f->file.in_len = 0;
    f->io.status = Ok;
    return Ok;
  }
}

void pU(char *p) {
  retCode ret = Ok;
  while (ret == Ok && *p != 0)
    ret = outChar(logFile, (codePoint) *p++);
  outChar(logFile, '\n');
  flushFile(logFile);
}
