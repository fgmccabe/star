//
// Created by Francis McCabe on 8/19/24.
//
#include "fiberP.h"
#include "buddy.h"

static void initFiberClass(classPo class, classPo req);
static void fiberInit(objectPo o, va_list *args);

static void fiberDestroy(objectPo o);

FiberClassRec FiberClass = {
  .objectPart = {
    (classPo) &ObjectClass,
    "fiber",
    initFiberClass,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    fiberDestroy,
    O_INHERIT_DEF,
    fiberInit,
    sizeof(FiberObjRecord),
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  .fiberPart = {}
};

static buddyRegionPo stackRegion;
integer stackRegionSize = (1 << 26);    /* 64M cells is default stack region */
integer minStackSize = 256;             /* What is the smallest stack size */

void initFiberClass(classPo class, classPo req)
{
  integer regionSize = (1 << lg2(stackRegionSize));

  stackRegion = createRegion(regionSize, minStackSize);
}

classPo fiberClass = (classPo) &FiberClass;

void fiberInit(objectPo o, va_list *args) {
  fiberPo f = O_FIBER(o);

  fiberFun runner = va_arg(*args, fiberFun);
  f->fiber.cl = va_arg(*args, void*);
  integer size = va_arg(*args, integer);
  f->fiber.size = size = (1 << lg2Ceiling(size)) - 1; // Adjust stack size to be just under a power of two
  f->fiber.memory = allocateBuddy(stackRegion, size);
}

void fiberDestroy(objectPo o) {
  fiberPo v = O_FIBER(o);


}



fiberPo fiber(int count, ...) {
  va_list args;

  va_start(args, count);

  fiberPo v = O_FIBER(makeObject(fiberClass, &args));



  va_end(args);

  return v;
}




fiberPo newFiber(fiberFun runner);

retCode suspendFiber(fiberPo thisFiber, fiberStatus msg, void *data);
retCode resumeFiber(fiberPo fiber, fiberStatus msg, void *data);
retCode retireFiber(fiberPo thisFbier, fiberStatus ret, void *data);
