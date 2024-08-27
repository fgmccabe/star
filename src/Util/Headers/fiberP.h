//
// Created by Francis McCabe on 8/19/24.
//

#ifndef STAR_FIBERP_H
#define STAR_FIBERP_H

#include "fiber.h"
#include "objectP.h"
#include <setjmp.h>

typedef struct {
  jmp_buf env;                          // Jump buffer for switching
  void *cl;                             // Client data
  void *memory;                         // This fiber's stack memory
  integer size;                         // And its size
} FiberObjectRec;

typedef struct fiber_record_ {
  ObjectRec object;                     /* object level of the fiber structure */
  FiberObjectRec fiber;
} FiberObjRecord;

typedef struct {
} FiberClassPart;

typedef struct fiber_class_ {
  ObjectClassRec objectPart;
  FiberClassPart fiberPart;
} FiberClassRec;

extern FiberClassRec FiberClass;

void initJumpBuff(fiberPo f,fiberFun fn);

#ifdef VERIFY_OBJECT
#define O_FIBER(c) ((fiberPo)(checkCast((c),fiberClass)))
#else
#define O_FIBER(c) ((fiberPo)(c))
#endif

#endif //STAR_FIBERP_H
