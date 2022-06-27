//
// Created by Francis McCabe on 6/24/22.
// Micro tasks are scheduleable tasks that represent external connections to the world

#ifndef STAR_MICROTASK_H
#define STAR_MICROTASK_H

#include "config.h"
#include "object.h"

typedef struct task_record_* taskPo;
extern classPo taskClass;

typedef enum {
  taskIdle,
  taskActive,
  taskCompleted,
  taskInError
} TaskState;

typedef retCode (*taskCBProc)(taskPo f);

taskPo createTask(taskCBProc onStart,taskCBProc onResume,taskCBProc onError,taskCBProc onCleanup,taskCBProc cb,void *cl);
void suspendTask(taskPo task);
void scheduleTask(taskPo task);
void killTask(taskPo task);

#ifdef VERIFY_OBJECT
#define O_TASK(c) ((taskPo)(checkCast((c),taskClass)))
#else
#define O_TASK(c) ((taskPo)(c))
#endif

#endif //STAR_MICROTASK_H
