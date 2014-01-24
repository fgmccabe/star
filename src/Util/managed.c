/* 
   Managed object implementation
   This is an abstract class -- would not normally be instantiated by itself
 
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
   
   Contact: Francis McCabe <mccabe@fla.fujitsu.com>
*/ 

#include "config.h"		/* Invoke configuration header */
#include "managedP.h"

#include <stdlib.h>
#include <assert.h>

static void initManagedClass(classPo class,classPo request);
static void manageObject(objectPo m,va_list *args);
static void unmanageObject(objectPo o);

ManagedClassRec ManagedClass = {
  {
    &ObjectClass,                         /* parent class is object */
    "managed",                            /* this is the managed class */
    NULL,                                 /* deal with inheritance */
    initManagedClass,                     /* MANAGED class initializer */
    O_INHERIT_DEF,                        /* MANAGED object element creation */
    unmanageObject,                       /* MANAGED objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    manageObject,			  /* initialization of an Io buffer */
    sizeof(ManagedObject), /* min size of an managed record -- should never use */
    NULL,				  /* pool of values for this class */
    PTHREAD_ONCE_INIT,			  /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL				/* the chain of managed objects */
  }
};

classPo managedClass = (classPo)&ManagedClass;

static void initManagedClass(classPo class,classPo request)
{
  ManagedClassRec *req = (ManagedClassRec*)request;

  req->managedPart.instances = NULL;
}

// Managed part of initialization -- does not consume any arguments from the constructor
static void manageObject(objectPo o,va_list *args){
  managedPo m = O_MANAGED(o);
  ManagedClassRec *class = (ManagedClassRec*)o->class;

  lockClass(o->class);

  if(class->managedPart.instances==NULL)
    class->managedPart.instances = m->managed.next = m->managed.prev = m;
  else{
    m->managed.next = class->managedPart.instances;
    m->managed.prev = class->managedPart.instances->managed.prev;
    class->managedPart.instances->managed.prev->managed.next = m;
    class->managedPart.instances->managed.prev = m;
    class->managedPart.instances = m;
  }
  unlockClass(o->class);
}

static void unmanageObject(objectPo o)
{

  managedPo m = O_MANAGED(o);
  ManagedClassRec *class = (ManagedClassRec*)o->class;

  lockClass(o->class);

  {
    managedPo next = m->managed.next;
    managedPo prev = m->managed.prev;

    next->managed.prev = prev;
    prev->managed.next = next;            /* We have unlinked this managed object */

    if(class->managedPart.instances == m){
      if(next!=m)
	class->managedPart.instances = next;
      else if(prev!=m)
	class->managedPart.instances = prev;
      else 
	class->managedPart.instances = NULL; /* The last managed object just disappeared*/
    }
  }

  unlockClass(o->class);
}

retCode processAll(classPo class,manageProc proc,void *cd)
{
  retCode ret = Ok;
  ManagedClassRec *managed = (ManagedClassRec*)class;

  lockClass(class);

  {
    managedPo m = managed->managedPart.instances;

    if(m!=NULL){
      do{
	ret = proc(m,cd);
	m = m->managed.next;
      }while(ret==Ok && m!=managed->managedPart.instances);
    }
  }

  unlockClass(class);
  return ret;
}

void destroyAll(classPo class)
{
  ManagedClassRec *managed = (ManagedClassRec*)class;
  
  lockClass(class);

  {
    while(managed->managedPart.instances!=NULL)
      destroyObject(O_OBJECT(managed->managedPart.instances));
  }
  unlockClass(class);
}

