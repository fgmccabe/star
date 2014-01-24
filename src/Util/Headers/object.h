/*
  Object base class header
  (c) 1999 F.G.McCabe

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
#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "config.h"
#include "logical.h"
#include "number.h"
#include "retcode.h"
#include <pthread.h>

typedef struct _object_ *objectPo;
typedef struct _class_ *classPo;

extern classPo objClass;

objectPo newObject(classPo c,...);
void destroyObject(objectPo o);

char *nameOfClass(classPo class);
logical isSubClass(classPo class,classPo parent);
classPo parentClass(classPo class);

classPo classOfObject(objectPo o);
logical isObjectOfClass(objectPo o,classPo parent);
logical isObject(objectPo o);

void lock(objectPo o);
retCode timedLock(objectPo o, number tmout);
void unlock(objectPo o);

void lockClass(classPo c);
void unlockClass(classPo c);

// A convenience hack
void initRecursiveMutex(pthread_mutex_t *mutex);

#ifdef VERIFY_OBJECT
#define O_OBJECT(c) ((objectPo)(checkCast((c),objClass)))
#else
#define O_OBJECT(c) ((objectPo)(c))
#endif

#endif
