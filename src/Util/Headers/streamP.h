//
// Created by Francis McCabe on 8/27/18.
//

#ifndef STAR_STREAMP_H
#define STAR_STREAMP_H

#include "stream.h"
#include "ioP.h"


typedef retCode (*streamProc)(streamPo f);

typedef struct {
  streamProc filler;                      // We use this to refill the buffer
} StreamClassPartRec;

typedef struct _stream_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;                // the io part of the class information */
  StreamClassPartRec streamPart;
} StreamClassRec;

extern StreamClassRec StreamClass;      /* the standard pointer to an Stream class record */

typedef struct _stream_part_ {          /* The stream specific part of a stream object */
  int fno;                              // The file number
  byte in_line[MAXLINE + 32];           // The input buffer */
  int16 in_pos;
  int16 in_len;

  long bufferPos;                       // Mark at beginning of this buffer

  byte out_line[MAXLINE];               // The output buffer
  int16 out_pos;                        // Current position within the output buffer
} StreamPart;

typedef struct _stream_object_ {
  ObjectRec object;                     // object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            // Io level of io object */
  StreamPart stream;                    // File level of file object
} StreamObject;


#endif //STAR_STREAMP_H
