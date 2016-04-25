/*
  Managed memory base
  (c) 2016 F.G.McCabe


 */
#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "config.h"
#include "logical.h"
#include "retcode.h"
#include <pthread.h>

typedef struct _object_ *objectPo;
typedef struct _class_ *classPo;

extern classPo objClass;

objectPo newObject(classPo c,...);
void destroyObject(objectPo o);

void incReference(objectPo o);
void decReference(objectPo o);

char *nameOfClass(classPo class);
logical isSubClass(classPo class,classPo parent);
classPo parentClass(classPo class);

classPo classOfObject(objectPo o);
logical isObjectOfClass(objectPo o,classPo parent);
logical isObject(objectPo o);

void lock(objectPo o);
retCode timedLock(objectPo o, double tmout);
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
