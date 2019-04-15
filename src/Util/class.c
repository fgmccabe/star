/*
  * Class handling -- meta level
  * Copyright (c) 2016, 2017. Francis G. McCabe
 */

#include "objectP.h"
#include <stdio.h>
#include <assert.h>

/* Meta-level aspects of object manipulation */

static classPo classToInit;

static void revInit(classPo cl,classPo class)
{
  if(cl->parent!=NULL)
    revInit(cl->parent,class);

  if(cl->classInit!=NULL&& cl->classInit!=O_INHERIT_DEF)
    cl->classInit(cl,class);
}

static void initClass(void)
{
  classPo cl = classToInit;

  while(cl!=NULL){
    if(cl->classInherit!=NULL && cl->classInherit!=O_INHERIT_DEF)
      cl->classInherit(cl,classToInit);
    cl = cl->parent;
  }

  revInit(classToInit,classToInit);
}

/* Initialize an object by calling all the initializers of its class */
static void initObject(classPo class,objectPo o,va_list *args)
{
  if(class->parent!=NULL)
    initObject(class->parent,o,args);

  if(class->init!=NULL)
    class->init(o,args);
}

/* Create a new object */
objectPo newObject(classPo class,...)
{
  va_list args;

  va_start(args,class);

  return makeObject(class,&args);
}

objectPo makeObject(classPo class,va_list *args)
{
  classToInit = class;
  pthread_once(&class->inited,initClass);

  lockClass(class);			/* We sync access to the class */

  {
    objectPo o = class->create(class);

    unlockClass(class);			/* and release it after created */

    initObject(class,o,args);

    return o;
  }
}

void destroyObject(objectPo o)
{
  assert(o->refCount<=0);

  classPo class = o->class;

  while(class!=NULL){
    if(class->destroy!=NULL){
      lockClass(class);
      class->destroy(o);
      unlockClass(class);
    }
    class = class->parent;
  }

  class = o->class;
  
  while(class!=NULL){		/* we use the first erasure method we find */
    if(class->erase!=NULL && class->erase!=O_INHERIT_DEF){
      lockClass(class);
      class->erase(o);
      unlockClass(class);
      return;
    }
    class = class->parent;
  }
}

char *nameOfClass(classPo class)
{
  return class->className;
}

classPo classOfObject(objectPo o)
{
  return o->class;
}

logical isSubClass(classPo class,classPo parent)
{
  if(class==parent)
    return True;
  else if(class->parent!=NULL)
    return isSubClass(class->parent,parent);
  else
    return False;
}

classPo parentClass(classPo class)
{
  return class->parent;
}

logical objectHasClass(objectPo o, classPo parent)
{
  return isSubClass(o->class,parent);
}

logical isObject(objectPo o)
{
  return isSubClass(o->class,objClass);
}

objectPo checkCast(void *c,classPo class)
{
  objectPo o = (objectPo)c;

  assert(isSubClass(o->class,class));

  return o;
}



