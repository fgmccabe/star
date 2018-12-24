/* 
  I/O handling library, common framework module
  This is an abstract class -- cannot be instantiated by itself
 
  Copyright (c) 2016, 2017, 2018. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "ioP.h"
#include <stdlib.h>
#include <unistrP.h>
#include <memory.h>
#include <assert.h>

static void initIoClass(classPo class, classPo request);

static void inheritIo(classPo class, classPo request);

static void ioClose(objectPo o);

static void IoInit(objectPo o, va_list *args);

static retCode nullInBytes(ioPo f, byte *ch, integer count, integer *actual);

static retCode nullOutBytes(ioPo f, byte *b, integer count, integer *actual);

static retCode nullOutByte(ioPo f, byte b);

static retCode nullEof(ioPo f);

static retCode nullReady(ioPo f);

static retCode nullFlusher(ioPo f, long count);

static retCode nullSeek(ioPo f, integer count);

static retCode nullClose(ioPo f);

IoClassRec IoClass = {
  {
    (classPo) &LockedClass,               /* parent class is object */
    "io",                                 /* this is the io class */
    inheritIo,                            /* deal with inheritance */
    initIoClass,                          /* IO class initializer */
    O_INHERIT_DEF,                        /* IO object element creation */
    ioClose,                              /* IO objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    IoInit,                               /* initialization of an Io buffer */
    sizeof(IoObject),                     /* min size of an io record -- should never use */
    NULL,                                  /* pool of values for this class */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,        /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    nullInBytes,          /* inByte, abstract for the io class  */
    nullOutBytes,         /* outByte, abstract for the io class  */
    nullOutByte,          /* putbackByte, abstract for the io class  */
    nullEof,              /* are we at end of file? */
    nullReady,            /* readyIn, abstract for the io class  */
    nullReady,            /* readyOut, abstract for the io class  */
    nullFlusher,          /* flush, abstract for the io class  */
    nullClose             /* close, abstract for the io class  */
  }
};

classPo ioClass = (classPo) &IoClass;

static void inheritIo(classPo class, classPo request) {
  IoClassRec *req = (IoClassRec *) request;
  IoClassRec *template = (IoClassRec *) class;
  logical done = False;

  while (!done) {
    done = True;

    if (req->ioPart.read == O_INHERIT_DEF) {
      if (template->ioPart.read != O_INHERIT_DEF)
        req->ioPart.read = template->ioPart.read;
      else
        done = False;
    }

    if (req->ioPart.backByte == O_INHERIT_DEF) {
      if (template->ioPart.backByte != O_INHERIT_DEF)
        req->ioPart.backByte = template->ioPart.backByte;
      else
        done = False;
    }

    if (req->ioPart.write == O_INHERIT_DEF) {
      if (template->ioPart.write != O_INHERIT_DEF)
        req->ioPart.write = template->ioPart.write;
      else
        done = False;
    }

    if (req->ioPart.isAtEof == O_INHERIT_DEF) {
      if (template->ioPart.isAtEof != O_INHERIT_DEF)
        req->ioPart.isAtEof = template->ioPart.isAtEof;
      else
        done = False;
    }

    if (req->ioPart.inReady == O_INHERIT_DEF) {
      if (template->ioPart.inReady != O_INHERIT_DEF)
        req->ioPart.inReady = template->ioPart.inReady;
      else
        done = False;
    }

    if (req->ioPart.outReady == O_INHERIT_DEF) {
      if (template->ioPart.outReady != O_INHERIT_DEF)
        req->ioPart.outReady = template->ioPart.outReady;
      else
        done = False;
    }

    if (req->ioPart.flush == O_INHERIT_DEF) {
      if (template->ioPart.flush != O_INHERIT_DEF)
        req->ioPart.flush = template->ioPart.flush;
      else
        done = False;
    }

    if (req->ioPart.close == O_INHERIT_DEF) {
      if (template->ioPart.close != O_INHERIT_DEF)
        req->ioPart.close = template->ioPart.close;
      else
        done = False;
    }

    template = (IoClassRec *) (template->objectPart.parent);
  }
}

static pthread_once_t ioOnce = PTHREAD_ONCE_INIT;

static void initIoEtc(void) {
  atexit(closeIo);                      /* set up general closer for exit */
  initRecursiveMutex(&ioClass->mutex);
}

static void initIoClass(classPo class, classPo request) {
  pthread_once(&ioOnce, initIoEtc);
}

static ioPo activeSet = NULL;

static void IoInit(objectPo o, va_list *args) {
  ioPo f = O_IO(o);
  char *name = va_arg(*args, char *);

  lockClass(ioClass);

  if (activeSet == NULL)
    activeSet = f->io.next = f->io.prev = f;
  else {
    f->io.next = activeSet;
    f->io.prev = activeSet->io.prev;
    activeSet->io.prev->io.next = f;
    activeSet->io.prev = f;
    activeSet = f;
  }

  uniCpy(f->io.filename, NumberOf(f->io.filename), name);
  f->io.status = Ok;
  f->io.inBpos = 0;
  f->io.inCpos = 0;
  f->io.outBpos = 0;
  f->io.currColumn = 0;
  f->io.encoding = unknownEncoding;

  unlockClass(ioClass);
}

static void ioClose(objectPo o) {
}

void closeIo(void) {
  flushOut();

  lockClass(ioClass);
  while (activeSet != NULL) {
    closeFile(activeSet);
  }
  unlockClass(ioClass);
}

/* Byte level input on Io buffers */
retCode inByte(ioPo f, byte *b) {
  byte buff[1];
  integer act;
  retCode ret = inBytes(f, &buff[0], 1, &act);

  if (ret == Ok) {
    if (act == 1) {
      *b = buff[0];
      return Ok;
    } else
      return Fail;
  }
  return ret;
}

retCode inBytes(ioPo f, byte *ch, integer count, integer *actual) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.read(f, ch, count, actual);
  f->io.inBpos += *actual;
  unlock(O_LOCKED(o));

  return ret;
}

retCode putBackByte(ioPo f, byte b) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.backByte(f, b);

  if (ret == Ok)
    f->io.inBpos--;

  unlock(O_LOCKED(o));
  return ret;
}

// Utility to skip shell preamble at start of file
retCode skipShellPreamble(ioPo f) {
  codePoint ch;
  retCode ret = inChar(f, &ch);

  if (ret == Ok) {
    if (ch == '#') {      /* look for standard #!/.... header */
      ret = inChar(f, &ch);
      if (ret == Ok && ch == '!') {
        while ((inChar(f, &ch)) == Ok && ch != uniEOF &&
               ch != '\n');              /* consume the interpreter statement */
      } else {
        unGetChar(f, ch);
        unGetChar(f, '#');
      }
    } else
      unGetChar(f, ch);
  }
  return ret;
}

/* Byte level output */

retCode outBytes(ioPo f, byte *data, integer len, integer *actual) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.write(f, data, len, actual);
  f->io.outBpos += *actual;

  unlock(O_LOCKED(o));
  return ret;
}

retCode outBlock(ioPo f, byte *data, integer len) {
  integer actual;
  retCode ret = outBytes(f, data, len, &actual);

  if (ret == Ok && len != actual)
    return ioErrorMsg(f, "couldnt write block of %d bytes properly to %s", len, fileName(f));
  else
    return ret;
}

retCode outByte(ioPo f, byte c) {
  byte buff[1] = {c};
  integer len = NumberOf(buff);
  retCode ret = outBytes(f, &buff[0], len, &len);

  if (ret == Ok && len != NumberOf(buff))
    return ioErrorMsg(f, "couldnt write byte properly to %s", fileName(f));
  else
    return ret;
}

static retCode adjustCharInCount(ioPo io, codePoint ch) {
  if (ch == '\n') {
    io->io.currColumn = 0;
  } else
    io->io.currColumn++;
  return Ok;
}

/* Character level input */
retCode inChar(ioPo io, codePoint *ch) {
  switch (io->io.encoding) {
    case utf8Encoding: {
      byte b;

      retCode ret = inByte(io, &b);

      if (ret == Ok) {
        if (b <= 0x7fu) {
          *ch = ((codePoint) (b & 0xffu));
          return adjustCharInCount(io, *ch);
        } else if (UC80(b)) {
          byte nb;
          ret = inByte(io, &nb);

          if (ret == Ok) {
            codePoint c = (codePoint) (UX80(b) << 6u | UXR(nb));
            *ch = c;
            return adjustCharInCount(io, c);
          } else {
            putBackByte(io, b);         /* this will allow us to restart the inChar */
            return ret;
          }
        } else if (UC800(b)) {
          byte up, md;

          ret = inByte(io, &md);
          if (ret == Ok) {
            ret = inByte(io, &up);

            if (ret == Ok) {
              *ch = (codePoint) ((UX800(b) << 12) | (UXR(md) << 6) | (UXR(up)));
              return adjustCharInCount(io, *ch);
            } else {
              putBackByte(io, md);
              putBackByte(io, b);
              return ret;
            }
          } else {
            putBackByte(io, b);
            return ret;
          }
        } else {
          return Error;
        }
      } else
        return ret;
    }

    default: {
      byte b;
      retCode ret = inByte(io, &b);

      if (ret == Ok) {
        *ch = ((codePoint) b) & 0xff;
        return adjustCharInCount(io, *ch);
      } else
        return ret;
    }
  }
}

retCode unGetChar(ioPo io, codePoint ch)   /* put a single character back */
{
  if (ch != uniEOF) {
    char chbuff[8];                       /* We are going to re-encode the byte */
    integer len = 0;
    retCode ret = Ok;

    switch (io->io.encoding) {
      case utf8Encoding:
        ret = appendCodePoint(&chbuff[0], &len, NumberOf(chbuff), ch);
        break;
      default:
        chbuff[0] = (byte) ch;
        len = 1;
        break;
    }

    while (ret == Ok && len-- > 0) {
      ret = ((IoClassRec *) io->object.class)->ioPart.backByte(io, (byte) chbuff[len]);
    }
    return ret;
  } else
    return Eof;
}

// Push a string back into the input channel.
// String is assumed to be allocated in order of arrival, so its pushed back in reverse order
retCode pushBack(ioPo f, char *str, integer from, integer len) {
  if (f != NULL) {
    if (from < len) {
      codePoint ch;
      retCode ret = nxtPoint(str, &from, len, &ch);
      if (ret == Ok) {
        ret = pushBack(f, str, from, len);
        if (ret == Ok)
          ret = unGetChar(f, ch);
      }
      return ret;
    } else
      return Ok;
  } else
    return Error;
}

/*
 * read a line ... up to a terminating character 
 * len should be at least 2 ... one for the final NULL byte
 */
retCode inLine(ioPo f, bufferPo buffer, char *term) {
  retCode ret = Ok;
  integer tlen = uniStrLen(term);
  objectPo o = O_OBJECT(f);
  clearBuffer(buffer);

  lock(O_LOCKED(o));

  if ((f->io.mode & ioREAD) != 0) {
    while (ret == Ok) { /* we need at least one char for the NULL */
      codePoint ch;
      ret = inChar(f, &ch);

      if (ret == Ok) {
        ret = outChar(O_IO(buffer), ch);

        if (uniIndexOf(term, tlen, 0, ch) >= 0)  /* have we found a terminating byte? */
          break;
      }
    }
  } else
    ret = Error;

  unlock(O_LOCKED(o));
  return ret;
}

/* Character-level output */

retCode outChar(ioPo io, codePoint ch) {
  char chbuff[8];                       /* We are going to re-encode the byte */
  integer len = 0;
  integer actual;
  retCode ret = Ok;

  switch (io->io.encoding) {
    case utf8Encoding:
      ret = appendCodePoint(&chbuff[0], &len, NumberOf(chbuff), ch);
      break;
    default:
      chbuff[0] = (byte) ch;
      len = 1;
      break;
  }

  if (ret == Ok) {
    ret = ((IoClassRec *) io->object.class)->ioPart.write(io, (byte *) chbuff, len, &actual);
    io->io.outBpos += actual;
    return ret;
  } else
    return ret;
}

retCode outText(ioPo f, const char *text, integer len) {
  integer remaining = len;
  integer pos = 0;
  retCode ret = Ok;

  while (ret == Ok && remaining > 0) {
    integer count;
    ret = ((IoClassRec *) f->object.class)->ioPart.write(f, (byte *) &text[pos], remaining, &count);
    remaining -= count;
    pos += count;
    f->io.outBpos += count;
  }

  return ret;
}

retCode outStr(ioPo f, char *str) {
  return outText(f, str, uniStrLen(str));
}

retCode outStrg(ioPo f, strgPo str) {
  assert(str != Null);
  return outText(f, strgVal(str), strgLen(str));
}

retCode flushFile(ioPo f)               /* generic file flush */
{
  objectPo o = O_OBJECT(f);
  retCode ret = Ok;

  lock(O_LOCKED(o));

  if (isWritingFile(f) == Ok)
    ret = ((IoClassRec *) f->object.class)->ioPart.flush(f, 0);

  unlock(O_LOCKED(o));
  return ret;
}

retCode preFlushFile(ioPo f, int count) /* file flush */
{
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.flush(f, count);
  unlock(O_LOCKED(o));

  return ret;
}

void flushOut(void)                     /* flush all files */
{
  lockClass(ioClass);

  if (activeSet != NULL) {
    ioPo f = activeSet;

    do {
      f = f->io.next;
      if ((f->io.mode & ioWRITE) != 0) {

        objectPo o = O_OBJECT(f);
        lock(O_LOCKED(o));

        while (flushFile(f) == Fail);
        unlock(O_LOCKED(o));
      }
    } while (f != activeSet);
  }
  unlockClass(ioClass);
}

/* File opening is specific to the type of file being opened, 
 * but all files can be closed using the same function
 */

retCode closeFile(ioPo f) {
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));

  if (--(f->object.refCount) <= 0) {
    while (flushFile(f) == Fail);
    //    clearFileProperties(f);     // clear out any attached properties

    lockClass(ioClass);

    if (f == activeSet) {
      if (f->io.next == f && f->io.prev == f)
        activeSet = NULL;    /* no more active files */
      else
        activeSet = f->io.next;  /* move the base pointer along */
    }

    f->io.next->io.prev = f->io.prev;
    f->io.prev->io.next = f->io.next;

    unlockClass(ioClass);
    unlock(O_LOCKED(o));

    return ((IoClassRec *) f->object.class)->ioPart.close(f);
  }

  unlock(O_LOCKED(o));
  return Ok;
}

void triggerIo(filterProc filter, void *cl) {
  lockClass(ioClass);

  if (activeSet != NULL) {
    ioPo f = activeSet;

    do {
      f = f->io.next;

      filter(f, cl);
    } while (f != activeSet);
  }

  unlockClass(ioClass);
}

static retCode nullInBytes(ioPo f, byte *ch, integer count, integer *actual) {
  return Error;
}

static retCode nullOutBytes(ioPo f, byte *b, integer count, integer *actual) {
  return Error;
}

static retCode nullOutByte(ioPo f, byte b) {
  return Error;
}

static retCode nullFlusher(ioPo f, long count) {
  return Ok;
}

static retCode nullClose(ioPo f) {
  return Ok;
}

static retCode nullEof(ioPo f) {
  return Error;
}

static retCode nullReady(ioPo f) {
  return Error;
}

retCode isFileAtEof(ioPo f)    /* Eof if at end of file */
{
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));

  retCode ret = ((IoClassRec *) f->object.class)->ioPart.isAtEof(f);

  unlock(O_LOCKED(o));
  return ret;
}

retCode fileStatus(ioPo f) {
  retCode ret;
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));
  ret = f->io.status;
  unlock(O_LOCKED(o));

  return ret;
}

retCode setBufferStatus(ioPo f, retCode status) {
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));
  f->io.status = status;
  unlock(O_LOCKED(o));
  return status;
}

ioDirection fileMode(ioPo f) {
  objectPo o = O_OBJECT(f);
  ioDirection mode;

  lock(O_LOCKED(o));
  mode = f->io.mode;
  unlock(O_LOCKED(o));
  return mode;
}

char *fileName(ioPo f) {
  return f->io.filename;
}

integer inCPos(ioPo f) {
  objectPo o = O_OBJECT(f);
  integer cPos;

  lock(O_LOCKED(o));
  cPos = f->io.inCpos;
  unlock(O_LOCKED(o));
  return cPos;
}

integer outBPos(ioPo f) {
  objectPo o = O_OBJECT(f);
  integer bPos;

  lock(O_LOCKED(o));
  bPos = f->io.outBpos;
  unlock(O_LOCKED(o));
  return bPos;
}

retCode isFileOpen(ioPo f) {
  ioDirection mode = fileMode(f);

  if (mode != ioNULL)
    return Ok;
  else
    return Fail;
}

void setEncoding(ioPo f, ioEncoding encoding) {
  f->io.encoding = encoding;
}

retCode isReadingFile(ioPo f) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  if ((fileMode(f) & ioREAD) != 0)
    ret = Ok;
  else
    ret = Fail;
  unlock(O_LOCKED(o));
  return ret;
}

retCode isWritingFile(ioPo f) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  if ((fileMode(f) & ioWRITE) != 0)
    ret = Ok;
  else
    ret = Fail;
  unlock(O_LOCKED(o));
  return ret;
}

/* test that a file is ready without actually reading anything */
retCode isInReady(ioPo f) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) (f->object.class))->ioPart.inReady(f);
  unlock(O_LOCKED(o));

  return ret;
}

retCode isOutReady(ioPo f) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) (f->object.class))->ioPart.outReady(f);
  unlock(O_LOCKED(o));
  return ret;
}

retCode isLookingAt(ioPo f, char *prefix) {
  char *mark = prefix;
  retCode ret = Ok;
  byte ch = 0;

  while (ret == Ok && *mark != '\0') {
    ret = inByte(O_IO(f), &ch);
    if (ret == Ok) {
      if (ch != *mark) {
        putBackByte(f, ch);
        while (mark > prefix) {
          byte b = (byte) *--mark;
          putBackByte(f, b);
        }
        return Fail;
      } else
        mark++;
    }
  }
  return ret;
}
