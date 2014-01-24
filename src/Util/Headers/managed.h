/*
 * Managed object class -- the class keeps track of all created instances. 
   (c) 2004 F.G. McCabe

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
#ifndef _MANAGED_LIB_H_
#define _MANAGED_LIB_H_

#include "retcode.h"
#include "logical.h"
#include "object.h"

typedef struct _managed_object_ *managedPo;
extern classPo managedClass;

// A function type to apply to all managed instances
typedef retCode (*manageProc)(managedPo o,void *cd);

retCode processAll(classPo class,manageProc proc,void *cd);
void destroyAll(classPo class);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_MANAGED(c) ((managedPo)(checkCast((c),managedClass)))
#else
#define O_MANAGED(c) ((managedPo)(c))
#endif

#endif
