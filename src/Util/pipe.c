/*
  Pipe class
  (c) 1994-2004 and beyond F.G. McCabe
*/


#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <formio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "pipeP.h"

#ifdef HAVE_RLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef SYSV
#include <stropts.h>
#endif

/* Set up the pipe class */

static void initPipeClass(classPo class, classPo req);
static void PipeDestroy(objectPo o);
static void PipeInit(objectPo list, va_list *args);

PipeClassRec PipeClass = {
  .objectPart = {
    .parent = (classPo) &FileClass,                  // parent class is file object
    .className = "pipe",                               // this is the pipe class
    .classInit = initPipeClass,                        // Pipe class initializer, phase I
    .classInherit = O_INHERIT_DEF,
    .create = O_INHERIT_DEF,                        // Pipe object element creation
    .destroy = PipeDestroy,                          // Pipe object destruction
    .erase = O_INHERIT_DEF,                        // erasure
    .init = PipeInit,                             // initialization of a file object
    .size = sizeof(PipeObject),                   // size of a pipe object
    .hashCode = O_INHERIT_DEF,
    .equality = O_INHERIT_DEF,
    .pool = NULL,                                  // pool of file values
    .inited = PTHREAD_ONCE_INIT,                    // not yet initialized
    .mutex = PTHREAD_MUTEX_INITIALIZER
  },
  .lockPart = {
  },
  .ioPart = {
    .read = O_INHERIT_DEF,                        // inBytes
    .write = O_INHERIT_DEF,                       // outBytes
    .backByte =  O_INHERIT_DEF,                        // backByte
    .inputReady = O_INHERIT_DEF,
    .outputReady = O_INHERIT_DEF,
    .isAtEof = O_INHERIT_DEF,                        // atEof
    .close = O_INHERIT_DEF,                         // close
    .position = O_INHERIT_DEF,
    .seek = O_INHERIT_DEF
  },
  .filePart = {
    .filler = O_INHERIT_DEF,                      // Refill the pip
    .asyncFill = O_INHERIT_DEF,                   // Asynchronous fill
    .flush = O_INHERIT_DEF,                     // Flush the pipe
    .asyncFlush = O_INHERIT_DEF
  },
  .pipePart = {
  }
};

static classPo pipeClass = (classPo) &PipeClass;

logical isAPipe(objectPo p){
  return objectHasClass(p,pipeClass);
}

static void initPipeClass(classPo class, classPo req) {
}

static void PipeInit(objectPo o, va_list *args) {
  pipePo p = O_PIPE(o);
  int child = va_arg(*args, int);
  pipePo sibling = va_arg(*args, pipePo);

  p->pipe.childProcess = child;

  if (sibling != NULL) {
    p->pipe.next = sibling->pipe.next;
    p->pipe.next->pipe.prev = p;
    sibling->pipe.next = p;
    p->pipe.prev = sibling;
  } else
    p->pipe.next = p->pipe.prev = p;
}

static void PipeDestroy(objectPo o) {
  pipePo p = O_PIPE(o);
  if (p->pipe.childProcess >= 0) {
    if (p->pipe.next != p || p->pipe.prev != p) {
      p->pipe.next->pipe.prev = p->pipe.prev; // unlink the pipe object
      p->pipe.prev->pipe.next = p->pipe.next;

      p->pipe.next = p->pipe.prev = NULL;
      p->pipe.childProcess = -1;        // we won't be the ones to kill the child off
    } else {
      int stat;

      if (waitpid(p->pipe.childProcess, &stat, 0) != p->pipe.childProcess)
        outMsg(logFile, "Problem when waiting for sub-process %d", p->pipe.childProcess);
    }
  }
}

/* Open a pipe */

static int maxOpenFds() {
#if HAVE_GETDTABLESIZE
  return getdtablesize();
#elif HAVE_GETRLIMIT
  return getrlimit(RLIMIT_NOFILE);
#elif HAVE_OPEN_MAX
  return OPEN_MAX;
#else
  return 64;
#endif
}

retCode openPipe(char *exec, char **argv, char **envv, ioPo *inpipe, ioPo *outpipe, ioPo *errpipe,
                 ioEncoding encoding) {
  int child, pipe1[2], pipe2[2], pipe3[2];
  char *name = exec;

  if (pipe(pipe1) < 0 || pipe(pipe2) < 0 || pipe(pipe3) < 0 || exec == NULL) {
    logMsg(logFile, "problem %s (%d) in opening pipe %s", strerror(errno), errno, exec);
    return Error;
  } else if ((child = fork()) < 0) {
    logMsg(logFile, "problem %s (%d) in forking %s", strerror(errno), errno, exec);
    return Error;
  } else if (child > 0) {    // We are the parent ...
    pipePo in = O_PIPE(newObject(pipeClass, name, pipe1[1], encoding, ioWRITE, child, NULL));
    pipePo out = O_PIPE(newObject(pipeClass, name, pipe2[0], encoding, ioREAD, child, in));
    pipePo err = O_PIPE(newObject(pipeClass, name, pipe3[0], encoding, ioREAD, child, out));

    *inpipe = O_IO(in);
    *outpipe = O_IO(out);
    *errpipe = O_IO(err);

    close(pipe1[0]);
    close(pipe2[1]);
    close(pipe3[1]);

    return Ok;
  } else {        /* We are the child */
    close(pipe1[1]);
    close(pipe2[0]);    // Close the unused portions
    close(pipe3[0]);

    if (dup2(pipe2[1], 1) < 0 || dup2(pipe2[1], 2) < 0 ||
        dup2(pipe1[0], 0) < 0 || dup2(pipe3[1], 2) < 0) {
      exit(-29);
    } else {
      int i, maxFds = maxOpenFds();

      for (i = 3; i < maxFds; i++)
        close(i);    // Close all other files ...

      if (execve(exec, argv, envv) < 0)  // Execute the program
        perror("Problem in executing");
    }
    return Ok;    // We won't get here
  }
}
