/*
  File I/O class
  Copyright (c) 2016, 2017. Francis G. McCabe
*/

#include "config.h"    // Invoke configuration header
#include "fileP.h"
#include "editline.h"
#include "io.h"

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <aio.h>
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

static void ioSignalHandler(int signal, siginfo_t *si, void *cl);

FileClassRec FileClass = {
  .objectPart={
    (classPo) &IoClass,                   // parent class is io object
    "file",                               // this is the file class
    initFileClass,                        // File class initializer, phase I
    inheritFileClass,
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
  .lockPart={},
  .ioPart={
    fileInBytes,                         // inByte
    fileEnqueueRead,                // Start a read operation
    fileOutBytes,                        // outBytes
    fileEnqueueWrite,               // Start a write operation
    fileBackByte,                     // put a byte back in the buffer
    fileAtEof,                          // Are we at end of file?
    fileFlusher,                          // flush
    fileClose                            // close
  },
  .filePart={
    fileConfigure,                        // configure a file
    flSeek,                               /* seek to a point in the file */
    fileInReady,                          // readyIn
    fileOutReady,                         // readyOut
    fileFill,                             // fill the file buffer
  }
};

classPo fileClass = (classPo) &FileClass;

// Implement inheritance of file specific portions of class

void initFileClass(classPo class, classPo request) {
  assert(request->pool != NULL);
  setupIOHandler(IO_SIGNAL, ioSignalHandler);
}

void inheritFileClass(classPo class, classPo request, classPo orig) {
  FileClassRec *req = (FileClassRec *) request;
  FileClassRec *template = (FileClassRec *) class;

  if (req->filePart.configure == O_INHERIT_DEF)
    req->filePart.configure = template->filePart.configure;

  if (req->filePart.seek == O_INHERIT_DEF)
    req->filePart.seek = template->filePart.seek;

  if (req->filePart.filler == O_INHERIT_DEF) {
    req->filePart.filler = template->filePart.filler;
  }

  if (req->filePart.inReady == O_INHERIT_DEF) {
    req->filePart.inReady = template->filePart.inReady;
  }

  if (req->filePart.outReady == O_INHERIT_DEF) {
    req->filePart.outReady = template->filePart.outReady;
  }

  if (req->ioPart.read == O_INHERIT_DEF) {
    req->ioPart.read = fileInBytes;
  }

  if (req->ioPart.async_read == O_INHERIT_DEF) {
    req->ioPart.async_read = fileEnqueueRead;
  }

  if (req->ioPart.backByte == O_INHERIT_DEF) {
    req->ioPart.backByte = fileBackByte;
  }

  if (req->ioPart.write == O_INHERIT_DEF) {
    req->ioPart.write = fileOutBytes;
  }

  if (req->ioPart.async_write == O_INHERIT_DEF) {
    req->ioPart.async_write = fileEnqueueWrite;
  }
  if (req->ioPart.isAtEof == O_INHERIT_DEF) {
    req->ioPart.isAtEof = fileAtEof;
  }

  if (req->ioPart.flush == O_INHERIT_DEF) {
    req->ioPart.flush = fileFlusher;
  }

  if (req->ioPart.close == O_INHERIT_DEF) {
    req->ioPart.close = fileClose;
  }
}

// IO initialization should already be done at this point
void FileInit(objectPo o, va_list *args) {
  filePo f = (filePo) o;

  // Set up the buffer pointers
  f->file.in_pos = 0;
  f->file.out_pos = 0;
  f->file.in_len = 0;
  f->file.fno = va_arg(*args, int);             // set up the file number
  configureIo(f, turnOnBlocking);
  setEncoding(O_IO(f), va_arg(*args, ioEncoding));     // set up the encoding
  f->io.mode = va_arg(*args, ioDirection);
}

retCode configureForAsynch(filePo fl, ioCallBackProc cb, void *cl) {
  retCode ret = configureIo(fl, enableAsynch);
  if (ret == Ok)
    ret = configureIo(fl, turnOffBlocking);
  if (ret == Ok) {
    fl->file.completionSignaler = cb;
    fl->file.ioReadClientData = cl;
  }
  return ret;
}

// Implement class file functions

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

logical fileInReady(filePo io) {
  filePo f = O_FILE(io);

  if (f->file.in_pos < f->file.in_len)
    return True;
  else {
    int fno = f->file.fno;

    if ((f->io.mode & ioREAD) != 0) {
      fd_set fdin;
      struct timeval period;

      FD_ZERO(&fdin);

      FD_SET(fno, &fdin);

      period.tv_sec = 0;
      period.tv_usec = 0;

      return (logical) (select(fno + 1, &fdin, NULL, NULL, &period) > 0);
    } else
      return False;
  }
}

logical fileOutReady(filePo io) {
  filePo f = O_FILE(io);

  if (f->file.out_pos < NumberOf(f->file.out_line))
    return True;
  else {
    int fno = f->file.fno;

    if ((f->io.mode & ioWRITE) != 0) {
      fd_set fdout;
      struct timeval period;

      FD_ZERO(&fdout);

      FD_SET(fno, &fdout);

      period.tv_sec = 0;
      period.tv_usec = 0;

      return (logical) (select(fno + 1, NULL, &fdout, NULL, &period) > 0);
    } else
      return False;
  }
}

void ioSignalHandler(int signal, siginfo_t *si, void *cl) {
  filePo f = O_FILE(cl);

  if (si->si_code == SI_ASYNCIO) {
    logMsg(logFile, "I/O Completion signal received for %s\n", fileName(O_IO(f)));
    if (f->file.completionSignaler != Null) {
      f->file.completionSignaler(O_IO(f), f->file.ioReadClientData);
    }
  }
}

retCode fileEnqueueRead(ioPo io, integer count, ioCallBackProc signaler, void *cl) {
  filePo f = O_FILE(io);

  if (f->file.ioReadClientData != Null)
    return Error;         // Already trying to read from this file
  else if (f->file.in_pos + count > f->file.in_len)
    return Space;
  f->file.ioReadClientData = cl;
  f->file.completionSignaler = signaler;
  memset(&f->file.aio, 0, sizeof(struct aiocb));
  f->file.aio.aio_fildes = f->file.fno;
  f->file.aio.aio_nbytes = count;
  f->file.aio.aio_reqprio = 0;
  f->file.aio.aio_offset = 0;
  f->file.aio.aio_sigevent.sigev_notify = SIGEV_SIGNAL;
  f->file.aio.aio_sigevent.sigev_signo = IO_SIGNAL;
  f->file.aio.aio_sigevent.sigev_value.sival_ptr = f;

  int s = aio_read(&f->file.aio);
  if (s == -1)
    return Error;
  return Ok;
}

retCode fileEnqueueWrite(ioPo f, byte *buffer, integer count, void *cl) {
  return Error;
}

retCode fileFlusher(ioPo io, long count) {
  filePo f = O_FILE(io);

  return fileFlush(f, count);
}

retCode flSeek(filePo f, integer count) {
  ioPo io = O_IO(f);
  flushFile(io);

  if (lseek(f->file.fno, count, SEEK_SET) == -1)
    return ioErrorMsg(io, "problem %s (%d) in positioning %s", strerror(errno), errno,
                      fileName(O_IO(f)));
  else {
    f->file.in_pos = f->file.out_pos = 0;
    f->file.in_len = 0;                 // ensure that we will be refilling

    if (isReadingFile(io)) {
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

ioPo logFile = Null;    /* File to write the log to */
ioPo stdIn = Null;
ioPo stdOut = Null;
ioPo stdErr = Null;

static sigset_t blocked;

static retCode trimName(char *s, integer sLen, char *front, char *trail, char *out, integer outLen);

static void stopAlarm(void)    // stop the cpu timer from delivering alarm signals
{
  sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, SIGVTALRM);  // The CPU timer

  sigprocmask(SIG_BLOCK, &set, &blocked);
}

static void startAlarm(void)                   // enable the virtual timer again
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
          return Interrupt;
        case EAGAIN:      // Not ready
          f->io.status = Fail;
          return Fail;
        default:        // Pretend its uniEOF
          f->file.in_pos = f->file.in_len = 0;
          return f->io.status = Eof;  // we have reach end of file
      }
    } else {
      f->file.in_pos = 0;
      f->file.in_len = (int16) len;
      f->file.bufferPos = f->io.inBpos;

      if (len == 0) {
        return f->io.status = Eof;
      } else {
        return f->io.status = Ok;
      }
    }
  } else
    return Ok;        // Already got stuff in there
}

retCode fileFlush(filePo f, long count) {
  int fno = f->file.fno;
  long written;
  long remaining = f->file.out_pos;
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
            f->file.out_pos = (int16) remaining;
          }
          return Interrupt;    // report an interrupt
        case EAGAIN:        // we shuffle the remaining buffer to the front
          if (writeGap > 0) {
            memmove(&f->file.out_line[0], cp, sizeof(byte) * remaining);
            f->file.out_pos = (int16) remaining;
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
        logMsg(logFile, "Warning: couldn't set ownership of fd %d", fno);
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

  if ((stat((const char *) fname, &buf) == -1) || S_ISDIR(buf.st_mode))
    return Fail;    /* File not found */;
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
#define FILE_ACCESS_MODE (F_OK|R_OK)

/* Check if a file is present or not */
retCode filePresent(char *name) {
  if (access((const char *) name, FILE_ACCESS_MODE) == 0)
    return Ok;
  else
    return Fail;
}

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

    stdIn = O_IO(newObject(fileClass, "stdin", fd, utf8Encoding, ioREAD));
    configureIo(O_FILE(stdIn), turnOnBlocking);
  }
  return stdIn;
}

ioPo Stdout(void) {
  if (stdOut == NULL) {
    char *stdoutName = "stdout";

    stdOut = O_IO(newObject(fileClass, stdoutName, 1, utf8Encoding, ioWRITE));
  }
  return stdOut;
}

ioPo Stderr(void) {
  if (stdErr == NULL) {
    char *stderrName = "stderr";

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

retCode fileSeek(filePo f, integer count) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((FileClassRec *) f->object.class)->filePart.seek(f, count);
  unlock(O_LOCKED(o));
  return ret;
}

/* test that a file is ready without actually reading anything */
logical isInReady(filePo f) {
  objectPo o = O_OBJECT(f);
  logical ret;

  lock(O_LOCKED(o));
  ret = ((FileClassRec *) (f->object.class))->filePart.inReady(f);
  unlock(O_LOCKED(o));

  return ret;
}

logical isOutReady(filePo f) {
  objectPo o = O_OBJECT(f);
  logical ret;

  lock(O_LOCKED(o));
  ret = ((FileClassRec *) (f->object.class))->filePart.outReady(f);
  unlock(O_LOCKED(o));
  return ret;
}

void pU(char *p) {
  retCode ret = Ok;
  while (ret == Ok && *p != 0)
    ret = outChar(logFile, (codePoint) *p++);
  outChar(logFile, '\n');
  flushFile(logFile);
}

// Utility to skip shell preamble at start of file
retCode skipShellPreamble(filePo f) {
  codePoint ch;
  ioPo io = O_IO(f);
  retCode ret = inChar(io, &ch);

  if (ret == Ok) {
    if (ch == '#') {      /* look for standard #!/.... header */
      ret = inChar(io, &ch);
      if (ret == Ok && ch == '!') {
        while ((inChar(io, &ch)) == Ok && ch != uniEOF &&
               ch != '\n');              /* consume the interpreter statement */
      } else {
        unGetChar(io, ch);
        unGetChar(io, '#');
      }
    } else
      unGetChar(io, ch);
  }
  return ret;
}

char *resolveFileName(char *base, const char *path, integer pathLen, char *buff, integer buffLen) {
  char wd[MAXFILELEN];
  char *fileNm = (char *) path;

  if (path[0] == '/') {
    uniNCpy(buff, buffLen, path, pathLen);
    return buff;
  } else if (path[0] == '~') {
    integer fstSlash = uniIndexOf(path, pathLen, 1, '/');
    if (fstSlash > 0) {
      char User[MAXFILELEN];
      if (fstSlash > 1)
        uniNCpy(User, NumberOf(User), path + 1, fstSlash - 1);
      else
        uniCpy(User, NumberOf(User), getenv("USER"));
      fileNm = (char *) &path[fstSlash + 1];
      pathLen -= fstSlash + 1;

      if (homeDir(User, wd, NumberOf(wd)) != Ok)
        return Null;
    }
  } else {
    uniTrim(base, uniStrLen(base), "", "/", wd, NumberOf(wd));
    fileNm = (char *) path;
  }

  char fname[MAXFILELEN];
  uniTrim(fileNm, pathLen, "", "/", fname, NumberOf(fname));
  pathLen = uniStrLen(fname);

  integer wdLen = uniStrLen(wd);
  integer pos = 0;

  while (pos < pathLen && fname[pos] == '.') {
    if (pos < pathLen - 2 && fname[pos + 1] == '.' && fname[pos + 2] == '/') {
      integer last = uniLastIndexOf(wd, wdLen, '/');
      if (last >= 0) {
        wdLen = last;
        wd[last] = '\0';
        pos += 3;
      } else
        break;
    } else if (pos < pathLen - 1 && fname[pos + 1] == '/') {
      pos += 2;
    } else if (pos == pathLen - 1)
      pos++;
    else
      break;
  }
  strMsg(buff, buffLen, "%s/%s", wd, &fname[pos]);
  return buff;
}

retCode resolvePath(char *root, integer rootLen, const char *fn, integer fnLen, char *buff, integer buffLen) {
  char wd[MAXFILELEN];
  char *fileNm = (char *) fn;

  if (fn[0] == '/') {
    return uniNCpy(buff, buffLen, fn, fnLen);
  } else if (fn[0] == '~')
    return Error;
  else {
    uniTrim(root, rootLen, "", "/", wd, NumberOf(wd));
  }

  char fname[MAXFILELEN];
  uniTrim(fileNm, fnLen, "", "/", fname, NumberOf(fname));
  fnLen = uniStrLen(fname);

  integer wdLen = uniStrLen(wd);
  integer pos = 0;

  while (pos < fnLen && fname[pos] == '.') {
    if (pos < fnLen - 2 && fname[pos + 1] == '.' && fname[pos + 2] == '/') {
      integer last = uniLastIndexOf(wd, wdLen, '/');
      if (last >= 0) {
        wdLen = last;
        wd[last] = '\0';
        pos += 3;
      } else
        return Error;
    } else if (pos < fnLen - 1 && fname[pos + 1] == '/') {
      pos += 2;
    } else if (pos == fnLen - 1)
      pos++;
    else
      break;
  }
  strMsg(buff, buffLen, "%s/%s", wd, &fname[pos]);
  return Ok;
}

retCode trimName(char *s, integer sLen, char *front, char *trail, char *out, integer outLen) {
  const integer frSize = uniStrLen(front);
  const integer trSize = uniStrLen(trail);

  integer fx = 0;
  integer tx = sLen;

  while (fx < sLen) {
    integer px = fx;
    codePoint ch = nextCodePoint(s, &px, sLen);
    if (uniIndexOf(front, frSize, 0, ch) >= 0) {
      fx = px;
      continue;
    } else
      break;
  }

  while (tx >= fx) {
    codePoint ch;
    integer px = tx;
    if (prevPoint(s, &px, &ch) == Ok) {
      if (uniIndexOf(trail, trSize, 0, ch) >= 0) {
        tx = px;
        continue;
      } else
        break;
    } else
      break;
  }

  return uniNCpy(out, outLen, &s[fx], tx - fx);
}
