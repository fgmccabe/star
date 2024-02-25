/* 
  I/O handling top-level
  Copyright (c) 2016, 2017, 2024 and beyond. Francis G. McCabe
*/
#ifndef _IO_LIB_H_
#define _IO_LIB_H_

#include "integer.h"
#include "object.h"
#include "lockable.h"
#include "strng.h"

typedef enum {
  ioNULL = 000u, ioREAD = 001u, ioWRITE = 002u
} ioDirection;

typedef struct io_object_ *ioPo;
extern classPo ioClass;

#include "unistr.h"
#include "stringBuffer.h"

#ifndef MAXLINE
#define MAXLINE 1024                  /* Size of a standard line buffer */
#endif

#ifndef MAXFILELEN
#define MAXFILELEN 512                /* Maximum length of a file name */
#endif

extern ioPo logFile;    /* The standard place to write logging msgs */

retCode inByte(ioPo f, byte *ch);      /* read a single byte -- with status */
retCode inBytes(ioPo f, byte *buffer, integer len, integer *actual); /* read a block of bytes */
retCode putBackByte(ioPo f, byte b);

retCode isLookingAt(ioPo f, const char *prefix);    /* Is prefix the first thing in the file? */

retCode isInputReady(ioPo io, integer count);
retCode isOutputReady(ioPo io, integer count);
retCode inChar(ioPo f, codePoint *ch);     /* read a character */
retCode unGetChar(ioPo f, codePoint ch);   /* put a single character back */
retCode inLine(ioPo f, strBufferPo buffer, char *term);

retCode pushBack(ioPo f, char *str, integer from, integer len);

retCode outByte(ioPo f, byte c);
retCode outChar(ioPo f, codePoint ch);
retCode outBytes(ioPo f, byte *data, integer len, integer *actual);

retCode outText(ioPo f, const char *text, integer len);
retCode outStr(ioPo f, char *str);
retCode outStrg(ioPo f, strgPo str);

integer ioPos(ioPo io);
retCode ioSeek(ioPo io,integer pos);

retCode closeIo(ioPo f);            /* generic file closer */

void setEncoding(ioPo f, ioEncoding encoding);
ioEncoding getEncoding(ioPo io);

logical isReadingFile(ioPo f);
logical isWritingFile(ioPo f);
retCode isFileAtEof(ioPo f);
retCode fileStatus(ioPo f);

char *fileName(ioPo f);
ioDirection fileMode(ioPo f);

retCode ioErrorMsg(ioPo io, char *fmt, ...);

#ifdef VERIFY_OBJECT
#define O_IO(c) ((ioPo)(checkCast((c),ioClass)))
#else
#define O_IO(c) ((ioPo)(c))
#endif

#endif
