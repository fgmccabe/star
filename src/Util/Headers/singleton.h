// A simple wrapper to allow an arbitrary C value to be brought into an object

#ifndef _SINGLETON_H
#define _SINGLETON_H

#include "object.h"

typedef struct _singleton_object_ *singletonPo;
extern classPo singletonClass;

singletonPo newSingleton(void *data);

void releaseSingleton(singletonPo singleton);

void *singletonVal(singletonPo s);

#ifdef VERIFY_OBJECT
#define O_SINGLETON(c) ((singletonPo)(checkCast((c),singletonClass)))
#else
#define O_SINGLETON(c) ((singletonPo)(c))
#endif

#endif
