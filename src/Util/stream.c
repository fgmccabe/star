//
// Created by Francis McCabe on 8/27/18.
//
#include <errno.h>
#include <string.h>
#include <signal.h>
#include "streamP.h"
#include "lineEdit.h"

static void inheritStream(classPo class, classPo request);
static void initStreamClass(classPo class, classPo request);
static void StreamInit(objectPo o, va_list *args);

static retCode streamInBytes(ioPo io, byte *ch, integer count, integer *actual);
static retCode streamOutBytes(ioPo io, byte *b, integer count, integer *actual);
static retCode streamBackByte(ioPo f, byte b);
static retCode streamAtEof(ioPo io);
static retCode streamInReady(ioPo io);
static retCode streamOutReady(ioPo io);
static retCode streamFlusher(ioPo io, long count);
static retCode streamClose(ioPo f);
static retCode streamFiller(streamPo f);

static retCode refillStream(streamPo f);

/* Set up the file class */

StreamClassRec StreamClass = {
  {
    (classPo) &IoClass,                   // parent class is io object
    "file",                               // this is the file class
    inheritStream,                        // file inheritance
    initStreamClass,                      // Stream class initializer, phase I
    O_INHERIT_DEF,                        // File object element creation
    NULL,                                 // File objectdestruction
    O_INHERIT_DEF,                        // erasure
    StreamInit,                           // initialization of a stream object
    sizeof(StreamObject),                 // size of a stream object
    O_INHERIT_DEF,                        // Hashcode for files
    O_INHERIT_DEF,                        // Equality for files
    NULL,                                 // pool of file values
    PTHREAD_ONCE_INIT,                    // not yet initialized
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    streamInBytes,                        // inByte
    streamOutBytes,                       // outBytes
    streamBackByte,                       // put a byte back in the buffer
    streamAtEof,                          // Are we at end of file?
    streamInReady,                        // readyIn
    streamOutReady,                       // readyOut
    streamFlusher,                        // flush
    streamClose                           // close
  },
  {
    streamFiller,                         // fill the file buffer
  }
};

classPo streamClass = (classPo) &StreamClass;

// Implement inheritance of file specific portions of class

void inheritStream(classPo class, classPo request) {
  StreamClassRec *req = (StreamClassRec *) request;
  StreamClassRec *template = (StreamClassRec *) class;

  logical done = False;

  while (!done) {
    done = True;

//    if (req->filePart.configure == O_INHERIT_DEF) {
//      if (template->filePart.configure != O_INHERIT_DEF)
//        req->filePart.configure = template->filePart.configure;
//      else
//        done = False;
//    }


    if (req->streamPart.filler == O_INHERIT_DEF) {
      if (template->streamPart.filler != O_INHERIT_DEF)
        req->streamPart.filler = template->streamPart.filler;
      else
        done = False;
    }

    template = (StreamClassRec *) (template->objectPart.parent);
  }
}

void initStreamClass(classPo class, classPo request) {
}

// IO initialization should already be done at this point
void StreamInit(objectPo o, va_list *args) {
  streamPo f = O_STREAM(o);

  // Set up the buffer pointers
  f->stream.in_pos = 0;
  f->stream.out_pos = 0;
  f->stream.in_len = 0;
  f->stream.fno = va_arg(*args, int);             // set up the file number
  //configureIo(O_IO(f), turnOnBlocking);
  setEncoding(O_IO(f), va_arg(*args, ioEncoding));     // set up the encoding
  f->io.mode = va_arg(*args, ioDirection);
}

retCode streamClose(ioPo io) {
  streamPo f = O_STREAM(io);
//  retCode ret = configureIo(f, turnOnBlocking);
  objectPo o = O_OBJECT(io);

  lock(O_LOCKED(o));

  retCode ret;

  while ((ret = flushFile(io)) == Fail);

  if (ret == Ok) {
    if (f->stream.fno >= 0) {
      if (close(f->stream.fno) < 0) {
        switch (errno) {
          case EINTR:
            ret = Interrupt;
            break;
          default:
            ret = ioErrorMsg(io, "problem %s (%d) in closing %s", strerror(errno), errno, fileName(io));
            break;
        }
      } else {
        f->stream.fno = -1;
        ret = Ok;
      }
    } else
      ret = Ok;        // probably already being closed
  }

  unlock(O_LOCKED(o));
  destroyObject(O_OBJECT(o)); // this will get rid of all the file objects attributes
  return ret;
}

retCode streamInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  streamPo f = O_STREAM(io);

  while (ret == Ok && remaining > 0) {
    if (f->stream.in_pos >= f->stream.in_len)
      ret = refillStream(f);
    if (ret == Ok) {
      *ch++ = f->stream.in_line[f->stream.in_pos++];
      remaining--;
    }
  }
  *actual = count - remaining;
  if (*actual > 0 && ret != Error)
    return Ok;
  else
    return ret;
}

retCode streamOutBytes(ioPo io, byte *b, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  streamPo f = O_STREAM(io);

  while (ret == Ok && remaining > 0) {
    if (f->stream.out_pos >= NumberOf(f->stream.out_line))
      ret = streamFlusher(io, 0);
    if (ret == Ok && f->stream.out_pos < NumberOf(f->stream.out_line)) {
      f->stream.out_line[f->stream.out_pos++] = *b++;
      remaining--;
    }
  }
  *actual = count - remaining;
  return ret;
}

retCode streamBackByte(ioPo io, byte b) {
  streamPo f = O_STREAM(io);

  if (f->stream.in_pos < 1) {
    if (f->stream.in_len - f->stream.in_pos < NumberOf(f->stream.in_line)) {
      memmove(&f->stream.in_line[1], &f->stream.in_line[f->stream.in_pos],
              sizeof(byte) * (f->stream.in_len - f->stream.in_pos));
      f->stream.in_line[0] = b;
      f->stream.in_len = (int16) (f->stream.in_len - f->stream.in_pos + 1);
      f->stream.in_pos = 0;
      f->io.status = Ok;
    } else
      return Error;
  } else {
    f->stream.in_pos--;
    f->stream.in_line[f->stream.in_pos] = b;
    f->io.status = Ok;
  }

  return Ok;
}

retCode streamAtEof(ioPo io) {
  streamPo f = O_STREAM(io);

  if (f->stream.in_pos < f->stream.in_len)
    return Ok;
  else
    return refillStream(f);
}

retCode streamInReady(ioPo io) {
  streamPo f = O_STREAM(io);

  if (f->stream.in_pos < f->stream.in_len)
    return Ok;
  else {
    int fno = f->stream.fno;

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

retCode streamOutReady(ioPo io) {
  streamPo f = O_STREAM(io);

  if (f->stream.out_pos < NumberOf(f->stream.out_line))
    return Ok;
  else {
    int fno = f->stream.fno;

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

retCode streamFlusher(ioPo io, long count) {
  streamPo f = O_STREAM(io);

  int fno = f->stream.fno;
  long written;
  int16 remaining = f->stream.out_pos;
  byte *cp = f->stream.out_line;
  long writeGap = 0;

  if (count > 0 && f->stream.out_pos + count < NumberOf(f->stream.out_line))
    return Ok;

  while (remaining > 0 && (written = write(fno, cp, (size_t) remaining)) != remaining) {
    if (written == -1) {
      switch (errno) {
        case 0:                           // Linux and/or solaris sometimes does this
          continue;
        case EINTR:
          if (writeGap > 0) {
            memmove(&f->stream.out_line[0], cp, sizeof(byte) * remaining);
            f->stream.out_pos = remaining;
          }
          return Interrupt;    // report an interrupt
        case EAGAIN:        // we shuffle the remaining buffer to the front
          if (writeGap > 0) {
            memmove(&f->stream.out_line[0], cp, sizeof(byte) * remaining);
            f->stream.out_pos = remaining;
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
  f->stream.out_pos = 0;
  f->io.status = Ok;
  return Ok;
}

static sigset_t blocked;

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

retCode streamFiller(streamPo f) {
  if (f->stream.in_pos >= f->stream.in_len) {  // nead to read more input?
    ssize_t len;
    int lerrno;                         // local copy of errno

    stopAlarm();      // Stop the time interrupt
    len = read(f->stream.fno, f->stream.in_line, (size_t) MAXLINE);
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
          f->stream.in_pos = f->stream.in_len = 0;
          return f->io.status = Eof;  // we have reach end of file
      }
    } else {
      f->stream.in_pos = 0;
      f->stream.in_len = (int16) len;
      f->stream.bufferPos = f->io.inBpos;

      if (len == 0) {
        return f->io.status = Eof;
      } else {
        return f->io.status = Ok;
      }
    }
  } else
    return Ok;        // Already got stuff in there
}

retCode refillStream(streamPo s) {
  objectPo o = O_OBJECT(s);
  retCode ret = Ok;

  lock(O_LOCKED(o));

  if (isReadingFile(O_IO(s)) == Ok)
    ret = ((StreamClassRec *) s->object.class)->streamPart.filler(s);
  else
    ret = Error;

  unlock(O_LOCKED(o));
  return ret;
}
