/*
  Object base class
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
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <errno.h>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include "objectP.h"
#include "timer.h"

/* Object base implementation class */

static void objectClassInit(classPo class,classPo request);
static objectPo objectCreate(classPo class);
static void objectDestroy(objectPo o);
static void objectErase(objectPo o);
static void objectInit(objectPo o,va_list *args);

ObjectClassRec ObjectClass = {
  NULL,                                 /* has no parent class */
  "object",
  NULL,
  objectClassInit,                      /* standard class initializer */
  objectCreate,                         /* object creation */
  objectDestroy,                        /* object destruction */
  objectErase,                          /* object removal */
  objectInit,                           /* object initialization */
  sizeof(ObjectRec),                    /* Each object's minimum size */
  NULL,					/* pool of values for this class */
  PTHREAD_ONCE_INIT,			/* not initialized yet */
  PTHREAD_MUTEX_INITIALIZER,		/* class lock */
};

classPo objClass = &ObjectClass;

static void objectClassInit(classPo class,classPo request)
{
  request->pool = newPool(request->size,8);
  if(request->create==O_INHERIT_DEF)
    request->create = class->create;
  if(request->erase==O_INHERIT_DEF)
    request->erase = class->erase;
  initRecursiveMutex(&request->mutex);
}

/* Generic object create function */
static objectPo objectCreate(classPo class)
{
  objectPo o = (objectPo)allocPool(class->pool);

  o -> class = class;
  return o;
}

void initRecursiveMutex(pthread_mutex_t *mutex)
{
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE);

  pthread_mutex_init(mutex,&attr);
  pthread_mutexattr_destroy(&attr);
}

static void objectInit(objectPo o,va_list *args)
{
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE);

  pthread_mutex_init(&o->mutex,&attr);
  pthread_mutexattr_destroy(&attr);
}


static void objectDestroy(objectPo o)
{
  pthread_mutex_destroy(&o->mutex);
}

/* Generic object erasure function */
static void objectErase(objectPo o)
{
  freePool(o->class->pool,o);
}

// Implement the object lock and release functions

void lock(objectPo o)
{
  pthread_mutex_lock(&o->mutex);
}

// We return Ok if we have the lock, 
// Fail if we timed out
// Error for other cases
// 

retCode timedLock(objectPo o,number tmout)
{
  if(fabs(tmout)<0.001){
    if(pthread_mutex_trylock(&o->mutex)!=0)
      return Fail;
    else
      return Ok;
  }
  else{
    setAlarm(tmout,NULL,NULL);
    switch(pthread_mutex_lock(&o->mutex)){
    case 0:
      cancelAlarm();
      return Ok;
    case ETIMEDOUT:
      cancelAlarm();
      return Fail;
    default:
      cancelAlarm();
      return Error;
    }
  }
}

void unlock(objectPo o)
{
  pthread_mutex_unlock(&o->mutex);
}

void lockClass(classPo c)
{
  pthread_mutex_lock(&c->mutex);
}

void unlockClass(classPo c)
{
  pthread_mutex_unlock(&c->mutex);
}


