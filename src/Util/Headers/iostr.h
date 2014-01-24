/*
  String I/O handling functions
  (c) 1994-1998 Imperial College & F.G. McCabe

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

  Contact: Francis McCabe <fmccabe@gmail.com>
*/

#ifndef _IO_STR_H_
#define _IO_STR_H_

#include "unicode.h"

typedef struct _string_object_ *stringPo;
extern classPo stringClass;

stringPo openStrInput(string name,uniChar *buffer,long len,ioEncoding encoding);
stringPo openStrOutput(uniChar *name,ioEncoding encoding);

// These are legacy
stringPo openInStr(uniChar *buffer,long len,ioEncoding encoding);
stringPo openOutStr(ioEncoding encoding);
stringPo openIoStr(ioEncoding encoding);
stringPo openBufferStr(uniChar *buffer,long len,ioEncoding encoding);

uniChar *strMsg(uniChar *buffer,long len,char *fmt,...);
uniChar *strAppend(uniChar *buffer,long len,char *fmt,...);
uniChar *getStrText(stringPo f,long *len);
retCode emptyOutStr(stringPo f);
retCode rewindStr(stringPo in);
long getStrPos(stringPo s);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_STRING(c) ((stringPo)(checkCast((c),stringClass)))
#else
#define O_STRING(c) ((stringPo)(c))
#endif

#endif
