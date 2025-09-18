//
// Implement singletons
//

#include "singletonP.h"

static void singletonInit(objectPo o, va_list *args);
static void singletonDestroy(objectPo o);
static integer singletonHash(objectPo o);
static logical singletonEquality(objectPo o1, objectPo o2);

SingletonClassRec SingletonClass = {
  {
    (classPo) &ObjectClass,
    "singleton",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    singletonDestroy,
    O_INHERIT_DEF,
    singletonInit,
    sizeof(SingletonObject),
    singletonHash,
    singletonEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo singletonClass = (classPo) &SingletonClass;

void singletonInit(objectPo o, va_list *args) {
  singletonPo s = O_SINGLETON(o);
  void *data = va_arg(*args, void *);
  s->s.data = data;
}

void singletonDestroy(objectPo o) {
  singletonPo s = O_SINGLETON(o);
  s->s.data = Null;
}

integer singletonHash(objectPo o) {
  singletonPo s = O_SINGLETON(o);
  return (integer)(s->s.data);
}

logical singletonEquality(objectPo o1, objectPo o2) {
  singletonPo s1 = O_SINGLETON(o1);
  singletonPo s2 = O_SINGLETON(o2);

  return (logical)(s1->s.data==s2->s.data);
}

singletonPo newSingleton(void *data) {
  return O_SINGLETON(newObject(singletonClass, data));
}

void *singletonVal(singletonPo s) {
  return s->s.data;
}

void releaseSingleton(singletonPo singleton){
  destroyObject(O_OBJECT(singleton));
}


