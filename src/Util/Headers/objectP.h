/*
  Object base class private header
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

#include "config.h"
#include "object.h"
#include "pool.h"
#include <stdarg.h>

#ifndef _OBJECT_P_H_
#define _OBJECT_P_H_

typedef void (*objectProc)(objectPo o);
typedef objectPo (*objectCreateProc)(classPo cl);
typedef void (*objectClassProc)(classPo class);
typedef void (*classInitProc)(classPo class,classPo request);

#ifndef O_INHERIT_DEF
#define O_INHERIT_DEF ((void*)-1)
#endif

typedef struct _object_ {
  classPo class;                        /* class of the object */
  pthread_mutex_t mutex;		/* Every object has a lock available */
} ObjectRec;

typedef struct _class_ {
  classPo parent;                       /* parent class of this object */
  char *className;                      /* name of this class */
  classInitProc classInherit;           /* procedure to set up inheritance */
  classInitProc classInit;            /* procedure to initialize classes */
  objectCreateProc create;              /* procedure to create an object */
  objectProc destroy;                   /* procedure to destroy an object */
  objectProc erase;                    /* procedure to remove object's memory */
  void (*init)(objectPo o,va_list *args); /* procedure to initialize an object */
  size_t size;                            /* size of an individual object */
  poolPo pool;                            /* pool of records for this class*/
  pthread_once_t inited;		/* This flag controls initialization */
  pthread_mutex_t mutex;		/* This allows a class-level lock */
} ObjectClassRec;

extern ObjectClassRec ObjectClass;

objectPo checkCast(void *c,classPo class);

#ifdef ALLTRACE
#ifndef VERIFY_OBJECT
#define VERIFY_OBJECT
#endif
#endif
#endif
