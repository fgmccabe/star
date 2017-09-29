/* 
   Managed object implementation
   This is an abstract class -- would not normally be instantiated by itself
 
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "managedP.h"

static void initManagedClass(classPo class, classPo request);
static void manageObject(objectPo m, va_list *args);
static void unmanageObject(objectPo o);

ManagedClassRec ManagedClass = {
  {
    (classPo)&LockedClass,                /* parent class is lockable */
    "managed",                            /* this is the managed class */
    NULL,                                 /* deal with inheritance */
    initManagedClass,                     /* MANAGED class initializer */
    O_INHERIT_DEF,                        /* MANAGED object element creation */
    unmanageObject,                       /* MANAGED objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    manageObject,                          /* initialization of an Io buffer */
    sizeof(ManagedObject),                /* min size of an managed record -- should never use */
    NULL,                                  /* pool of values for this class */
    O_INHERIT_DEF,                        // Hash function is inherited
    O_INHERIT_DEF,                        // Equality function is also inherited
    PTHREAD_ONCE_INIT,                    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    NULL        /* the chain of managed objects */
  }
};

classPo managedClass = (classPo) &ManagedClass;

static void initManagedClass(classPo class, classPo request) {
  ManagedClassRec *req = (ManagedClassRec *) request;

  req->managedPart.instances = NULL;
}

// Managed part of initialization -- does not consume any arguments from the constructor
static void manageObject(objectPo o, va_list *args) {
  managedPo m = O_MANAGED(o);
  ManagedClassRec *class = (ManagedClassRec *) o->class;

  lockClass(o->class);

  if (class->managedPart.instances == NULL)
    class->managedPart.instances = m->managed.next = m->managed.prev = m;
  else {
    m->managed.next = class->managedPart.instances;
    m->managed.prev = class->managedPart.instances->managed.prev;
    class->managedPart.instances->managed.prev->managed.next = m;
    class->managedPart.instances->managed.prev = m;
    class->managedPart.instances = m;
  }
  unlockClass(o->class);
}

static void unmanageObject(objectPo o) {

  managedPo m = O_MANAGED(o);
  ManagedClassRec *class = (ManagedClassRec *) o->class;

  lockClass(o->class);

  {
    managedPo next = m->managed.next;
    managedPo prev = m->managed.prev;

    next->managed.prev = prev;
    prev->managed.next = next;            /* We have unlinked this managed object */

    if (class->managedPart.instances == m) {
      if (next != m)
        class->managedPart.instances = next;
      else if (prev != m)
        class->managedPart.instances = prev;
      else
        class->managedPart.instances = NULL; /* The last managed object just disappeared*/
    }
  }

  unlockClass(o->class);
}

retCode processAll(classPo class, manageProc proc, void *cd) {
  retCode ret = Ok;
  ManagedClassRec *managed = (ManagedClassRec *) class;

  lockClass(class);

  {
    managedPo m = managed->managedPart.instances;

    if (m != NULL) {
      do {
        ret = proc(m, cd);
        m = m->managed.next;
      } while (ret == Ok && m != managed->managedPart.instances);
    }
  }

  unlockClass(class);
  return ret;
}

void destroyAll(classPo class) {
  ManagedClassRec *managed = (ManagedClassRec *) class;

  lockClass(class);

  {
    while (managed->managedPart.instances != NULL)
      destroyObject(O_OBJECT(managed->managedPart.instances));
  }
  unlockClass(class);
}

