//
// Created by Francis McCabe on 6/24/22.
//

#ifndef STAR_MICROTASKP_H
#define STAR_MICROTASKP_H

#include "microTask.h"
#include "objectP.h"

typedef struct {
  StackState state;
  taskCBProc onStart;
  taskCBProc onResume;
  taskCBProc onError;
  taskCBProc onCleanup;
  taskCBProc cb;
  void *cl;
} TaskObjectRec;

typedef struct task_record_ {
  ObjectRec object;                     /* object level of the stack structure */
  TaskObjectRec stack;                  // Task part of object
} TaskRecord;

typedef struct {

} TaskClassPart;

typedef struct task_class {
  ObjectClassRec objectPart;
  TaskClassPart taskPart;
} TaskClassRec;

extern TaskClassRec StackClass;

#endif //STAR_MICROTASKP_H
