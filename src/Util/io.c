/* 
  I/O handling library, common framework module
  This is an abstract class -- cannot be instantiated by itself
 
  Copyright (c) 2016, 2017, 2018, 2019. Francis G. McCabe
*/

#include "ioP.h"
#include "file.h"
#include <stdlib.h>
#include <unistrP.h>
#include <assert.h>

static void initIoClass(classPo class, classPo request);
static void inheritIOClass(classPo class, classPo request, classPo orig);
static void ioClose(objectPo o);
static void IoInit(objectPo o, va_list *args);
static retCode nullInBytes(ioPo f, byte *ch, integer count, integer *actual);
static retCode nullOutBytes(ioPo f, byte *b, integer count, integer *actual);
static retCode nullOutByte(ioPo f, byte b);
static retCode nullEof(ioPo f);
static retCode nullClose(ioPo f);
static integer nullPos(ioPo f);
static retCode nullSeek(ioPo f, integer px);
static retCode nullIsReady(ioPo f, integer cs);

IoClassRec IoClass = {
  .objectPart={
    .parent = (classPo) &LockedClass,                /* parent class is object */
    .className = "io",                                 /* this is the io class */
    .classInit = initIoClass,                          /* IO class initializer */
    .classInherit = inheritIOClass,                     // Inherit from IO class
    .create = O_INHERIT_DEF,                        /* IO object element creation */
    .destroy = ioClose,                              /* IO objectdestruction */
    .erase = O_INHERIT_DEF,                        /* erasure */
    .init = IoInit,                               /* initialization of an Io buffer */
    .size = sizeof(IoObject),                     /* min size of an io record -- should never use */
    .pool = NULL,                                  /* pool of values for this class */
    .hashCode =  O_INHERIT_DEF,                        // No special hash function
    .equality = O_INHERIT_DEF,                        // No special equality
    .inited = PTHREAD_ONCE_INIT,          /* not yet initialized */
    .mutex = PTHREAD_MUTEX_INITIALIZER
  },
  .lockPart={},
  .ioPart={
    .read =nullInBytes,                /* inByte, abstract for the io class  */
    .write =nullOutBytes,              /* outByte, abstract for the io class  */
    .backByte = nullOutByte,           /* putbackByte, abstract for the io class  */
    .isAtEof =nullEof,                 /* are we at end of file? */
    .inputReady = nullIsReady,         // Are we immediately able to read XX bytes?
    .outputReady = nullIsReady,
    .close = nullClose,                /* close, abstract for the io class  */
    .position = nullPos,               // Report io position
    .seek = nullSeek                   // Move io position
  }
};

classPo ioClass = (classPo) &IoClass;

static pthread_once_t ioOnce = PTHREAD_ONCE_INIT;

static void initIoEtc(void) {
  atexit(closeAllFiles);                      /* set up general closer for exit */
  initRecursiveMutex(&ioClass->mutex);
}

static void initIoClass(classPo class, classPo request) {
  pthread_once(&ioOnce, initIoEtc);
}

void inheritIOClass(classPo class, classPo request, classPo orig) {
  IoClassRec *req = (IoClassRec *) request;
  IoClassRec *template = (IoClassRec *) class;

  if (req->ioPart.read == O_INHERIT_DEF) {
    req->ioPart.read = template->ioPart.read;
  }

  if (req->ioPart.backByte == O_INHERIT_DEF) {
    req->ioPart.backByte = template->ioPart.backByte;
  }

  if (req->ioPart.write == O_INHERIT_DEF) {
    req->ioPart.write = template->ioPart.write;
  }

  if (req->ioPart.isAtEof == O_INHERIT_DEF) {
    req->ioPart.isAtEof = template->ioPart.isAtEof;
  }

  if (req->ioPart.close == O_INHERIT_DEF) {
    req->ioPart.close = template->ioPart.close;
  }

  if (req->ioPart.position == O_INHERIT_DEF) {
    req->ioPart.position = template->ioPart.position;
  }
  if (req->ioPart.seek == O_INHERIT_DEF) {
    req->ioPart.seek = template->ioPart.seek;
  }
}

static void IoInit(objectPo o, va_list *args) {
  ioPo f = O_IO(o);
  char *name = va_arg(*args, char *);

  lockClass(f->object.class);

  uniCpy(f->io.filename, NumberOf(f->io.filename), name);
  f->io.currColumn = 0;
  f->io.encoding = unknownEncoding;

  unlockClass(f->object.class);
}

static void ioClose(objectPo o) {
}

retCode isInputReady(ioPo io, integer count) {
  return ((IoClassRec *) io->object.class)->ioPart.inputReady(io, count);
}

retCode isOutputReady(ioPo io, integer count) {
  return ((IoClassRec *) io->object.class)->ioPart.outputReady(io, count);
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
  return ((IoClassRec *) f->object.class)->ioPart.read(f, ch, count, actual);
}

retCode putBackByte(ioPo f, byte b) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.backByte(f, b);

  unlock(O_LOCKED(o));
  return ret;
}

/* Byte level output */

retCode outBytes(ioPo f, byte *data, integer len, integer *actual) {
  objectPo o = O_OBJECT(f);
  retCode ret;

  lock(O_LOCKED(o));
  ret = ((IoClassRec *) f->object.class)->ioPart.write(f, data, len, actual);
  unlock(O_LOCKED(o));
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
              *ch = (codePoint) ((UX800(b) << 12u) | (UXR(md) << 6u) | (UXR(up)));
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
        *ch = ((codePoint) b) & 0xffu;
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
      case rawEncoding:
      default:
        chbuff[0] = (char) ch;
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
retCode inLine(ioPo f, strBufferPo buffer, char *term) {
  retCode ret = Ok;
  integer tlen = uniStrLen(term);
  objectPo o = O_OBJECT(f);
  clearStrBuffer(buffer);

  lock(O_LOCKED(o));

  if ((f->io.mode & ioREAD) != 0) {
    while (ret == Ok) { /* we need at least one char for the NULL */
      codePoint ch;
      ret = inChar(f, &ch);

      if (ret == Ok) {
        if (ch == 0)
          return Error;
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
      chbuff[0] = (char) ch;
      len = 1;
      break;
  }

  if (ret == Ok) {
    return ((IoClassRec *) io->object.class)->ioPart.write(io, (byte *) chbuff, len, &actual);
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

/* File opening is specific to the type of file being opened, 
 * but all files can be closed using the same function
 */

retCode closeIo(ioPo f) {
  return ((IoClassRec *) f->object.class)->ioPart.close(f);
}

retCode nullInBytes(ioPo f, byte *ch, integer count, integer *actual) {
  return Error;
}

retCode nullOutBytes(ioPo f, byte *b, integer count, integer *actual) {
  return Error;
}

retCode nullOutByte(ioPo f, byte b) {
  return Error;
}

retCode nullClose(ioPo f) {
  return Ok;
}

retCode nullEof(ioPo f) {
  return Error;
}

integer nullPos(ioPo f) {
  return -1;
}

retCode nullSeek(ioPo f, integer px) {
  return Error;
}

retCode nullIsReady(ioPo f, integer cs) {
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

integer ioPos(ioPo io) {
  return ((IoClassRec *) io->object.class)->ioPart.position(io);
}

retCode ioSeek(ioPo io, integer pos) {
  return ((IoClassRec *) io->object.class)->ioPart.seek(io, pos);
}

void setEncoding(ioPo f, ioEncoding encoding) {
  f->io.encoding = encoding;
}

ioEncoding getEncoding(ioPo io) {
  return io->io.encoding;
}

logical isReadingFile(ioPo f) {
  objectPo o = O_OBJECT(f);
  logical ret;

  lock(O_LOCKED(o));
  if ((fileMode(f) & ioREAD) != 0)
    ret = True;
  else
    ret = False;
  unlock(O_LOCKED(o));
  return ret;
}

logical isWritingFile(ioPo f) {
  objectPo o = O_OBJECT(f);
  logical ret;

  lock(O_LOCKED(o));
  if ((fileMode(f) & ioWRITE) != 0)
    ret = True;
  else
    ret = False;
  unlock(O_LOCKED(o));
  return ret;
}

static logical testChar(char mark, char ch, char *special) {
  if (mark == '\1') {
    if (*special == '\0') {
      *special = ch;
      return True;
    } else
      return (logical) (*special == ch);
  } else
    return (logical) (mark == ch);
}

retCode isLookingAt(ioPo f, const char *prefix) {
  char special = '\0';
  char *mark = (char *) prefix;
  retCode ret = Ok;
  byte ch = 0;

  while (ret == Ok && *mark != '\0') {
    ret = inByte(O_IO(f), &ch);
    if (ret == Ok) {
      if (!testChar(*mark, ch, &special)) {
        putBackByte(f, ch);
        while (mark > prefix) {
          byte b = (byte) *--mark;
          if (b == '\01')
            putBackByte(f, special);
          else
            putBackByte(f, b);
        }
        return Fail;
      } else
        mark++;
    }
  }
  return ret;
}
