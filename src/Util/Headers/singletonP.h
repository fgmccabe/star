// Singleton object class.

#ifndef _SINGLETON_P_H_
#define _SINGLETON_P_H_

#incude "singleton.h"
#include "retcode.h"
#include "logical.h"

/* Class definition types */
typedef struct {
  size_t elementSize;
} SingletonClassPartRec;

typedef struct _singleton_class_ {
  ObjectClassRec objectPart;
  SingletonClassPartRec singletonPart;
} SingletonClassRec;

/* Singleton Instance definition types */

typedef struct _singleton_part {
  void *data;
} SingletonRec;

typedef struct _singleton_object_ {
  ObjectRec object;                     // The object part of the singleton object
  SingletonRec singleton;                   // The singleton part of the singleton object
} SingletonObject;

extern SingletonClassRec SingletonClass;

extern classPo singletonClass;    /* The pointer to the singleton class */

#endif // _SINGLETON_P_H_
