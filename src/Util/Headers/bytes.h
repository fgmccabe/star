/*
  Byte String I/O handling functions for the Go! run-time system
  (c) 1994-2004 Imperial College & F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
*/

#ifndef _IO_BYTE_H_
#define _IO_BYTE_H_

#include "unicode.h"

typedef struct _byte_object_ *bytePo;
extern classPo byteClass;

ioPo openInByteStr(byte *buffer,long len,ioEncoding encoding);
ioPo openOutByteStr(ioEncoding encoding);

byte *byteMsg(byte *buffer,long len,char *fmt,...);
byte *getByteStr(bytePo f,long *len);
retCode emptyByteStr(bytePo f);
retCode rewindByteStr(bytePo in);
long getBytePos(bytePo s);
byte *byteMsg(byte *buffer,long len,char *fmt,...);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_BYTE(c) ((bytePo)(checkCast((c),byteClass)))
#else
#define O_BYTE(c) ((bytePo)(c))
#endif

#endif
