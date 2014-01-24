/*
   Pipeclass interface
   (c) 1994-2004 Imperial College and F.G. McCabe

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

#ifndef _IO_PIPE_LIB_H_
#define _IO_PIPE_LIB_H_

#include "config.h"
#include "file.h"

typedef struct _pipe_object_ *pipePo;

retCode openPipe(char *exec,char **argv,char **envv,ioPo *inpipe,ioPo *outpipe,ioPo *errpipe,
                 ioEncoding encoding);

#endif

