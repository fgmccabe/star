/*
 * Some system functions
 */
#include <sys/time.h>
#include <errno.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <globals.h>
#include <tpl.h>
#include <errorCodes.h>
#include <unistd.h>
#include <stringBuffer.h>
#include <arithP.h>
#include <pipe.h>
#include <ioops.h>
#include <iochnnlP.h>
#include <lblops.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "engine.h"
#include "arith.h"
#include "consP.h"


// Number of nano seconds
#define NANOS 1000000000

void sleep_for(integer amnt) {
  long nanos = amnt % NANOS;
  long secs = amnt / NANOS;
  struct timespec Amnt = {.tv_sec=secs, .tv_nsec=nanos};
  struct timespec SoFar;

  while (nanosleep(&Amnt, &SoFar) != 0) {
    switch (errno) {
      case ENOSYS:
        outMsg(logFile, "no nanosleep\n");
        exit(99);
      case EINTR:
        if (SoFar.tv_sec != 0 || SoFar.tv_nsec != 0)
          Amnt = SoFar;
        else
          return;
      default:
        outMsg(logFile, "problem in nanosleep");
        exit(99);
    }
  }
}

void memerr() {
  outMsg(logFile, "out of heap space\n");
  flushOut();
  exit(99);
}

ReturnStatus g__exit(processPo p, heapPo h, termPo arg1) {
  integer ix = integerVal(arg1);

  exit((int) ix);
}

static char **argsv = NULL;  /* Store the command line list */
static int argcnt = 0;

void init_args(char **argv, int argc, int start) {
  argsv = &argv[start];
  argcnt = argc - start;
}

termPo commandLine(heapPo H) {
  termPo list = (termPo) nilEnum;
  termPo el = (termPo) voidEnum;
  int root = gcAddRoot(H, &list);
  gcAddRoot(H, &el);

  for (integer ix = argcnt - 1; ix >= 0; ix--) {
    el = (termPo) allocateString(H, argsv[ix], uniStrLen(argsv[ix]));
    list = (termPo) allocateCons(H, el, list);
  }
  gcReleaseRoot(H, root);
  return list;
}

extern char **environ;

integer countEnviron() {
  integer ix = 0;
  for (; environ[ix] != Null; ix++);
  return ix;
}

ReturnStatus g__envir(processPo P, heapPo h) {
  integer cnt = countEnviron();
  termPo list = (termPo) nilEnum;
  int root = gcAddRoot(h, (ptrPo) &list);
  termPo ky = voidEnum;
  termPo vl = voidEnum;
  termPo pair = voidEnum;
  gcAddRoot(h, &ky);
  gcAddRoot(h, &vl);
  gcAddRoot(h, &pair);

  switchProcessState(P, in_exclusion);

  for (integer ix = 0; ix < cnt; ix++) {
    char *envPair = environ[ix];
    char *pt = strchr(environ[ix], '=');

    if (pt != NULL) {
      ky = (termPo) allocateString(h, envPair, pt - envPair);
      vl = (termPo) allocateString(h, pt + 1, uniStrLen(pt + 1));
    } else {
      ky = (termPo) allocateCString(h, envPair);
      vl = voidEnum;
    }
    pair = (termPo) allocatePair(h, ky, vl);
    list = (termPo) allocateCons(h, pair, list);
  }
  gcReleaseRoot(NULL, root);
  setProcessRunnable(P);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) list};
}

ReturnStatus g__getenv(processPo P, heapPo h, termPo a1, termPo a2) {
  char key[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(a1), key, NumberOf(key));

  char *val = getenv((char *) key);

  if (val != NULL) {
    return (ReturnStatus) {.ret=Ok,
      .result=(termPo) allocateCString(h, val)};
  } else {
    return (ReturnStatus) {.ret=Ok, .result=a2};
  }
}

ReturnStatus g__setenv(processPo P, heapPo h, termPo a1, termPo a2) {
  char key[MAX_SYMB_LEN];
  char val[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(a1), key, NumberOf(key));
  copyChars2Buff(C_STR(a2), val, NumberOf(val));

  if (setenv((char *) key, val, 1) == 0) {
    return (ReturnStatus) {.ret=Ok, .result=voidEnum};
  } else
    return liberror(P, h, "_setenv", eFAIL);
}

ReturnStatus g__repo(processPo p, heapPo h) {
  char repoBuffer[MAXFILELEN];
  strMsg(repoBuffer, NumberOf(repoBuffer), "%s/", repoDir);
  termPo repo = (termPo) allocateString(h, repoBuffer, uniStrLen(repoBuffer));

  return (ReturnStatus) {.result = repo, .ret=Ok};
}

ReturnStatus g__getlogin(processPo P, heapPo h) {
  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateCString(h, getlogin())};
}

ReturnStatus g__shell(processPo P, heapPo h, termPo a1, termPo a2, termPo a3) {
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(a1), cmd, NumberOf(cmd));

  termPo args = a2;
  termPo env = a3;

  integer argCnt = consLength(args);
  integer envCnt = consLength(env);

  if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
    setProcessRunnable(P);
    return liberror(P, h, "__shell", eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return liberror(P, h, "__shell", eNOPERM);
  } else {
    char **argv = (char **) calloc((size_t) (argCnt + 2), sizeof(char *));
    char **envp = (char **) calloc((size_t) (envCnt + 1), sizeof(char *));
    int pid;

    argv[0] = cmd;

    for (integer ix = 0; ix < argCnt; ix++) {
      char arg[MAXFILELEN];
      copyChars2Buff(C_STR(consHead(C_NORMAL(args))), arg, NumberOf(arg));
      args = consTail(C_NORMAL(args));
      argv[ix + 1] = strdup(arg);
    }

    argv[argCnt + 1] = NULL;

    for (integer ix = 0; ix < envCnt; ix++) {
      normalPo pair = C_NORMAL(consHead(C_NORMAL(env)));
      env = consTail(C_NORMAL(env));
      strBufferPo lineBf = newStringBuffer();

      integer klen, vlen;
      const char *key = strVal(nthArg(pair, 0), &klen);
      const char *val = strVal(nthArg(pair, 1), &vlen);

      outMsg(O_IO(lineBf), "%S = %S", key, klen, val, vlen);

      integer lineLen;
      const char *line = getTextFromBuffer(lineBf, &lineLen);
      envp[ix] = strndup(line, (size_t) lineLen);
      closeFile(O_IO(lineBf));
    }

    envp[envCnt] = NULL;

    switchProcessState(P, wait_child);  /* We are now waiting for a child */

    if ((pid = fork()) == 0) {
      // child process, terminating after execve
      execve((char *) cmd, argv, envp);
      // abnormal termination -- should never get here
      _exit(127);
    } else {
      // parent process (agent)
      for (integer ix = 1; argv[ix] != NULL; ix++)  // argv[0] is a local string
        free(argv[ix]);

      for (integer ix = 0; envp[ix] != NULL; ix++)
        free(envp[ix]);

      free(argv);
      free(envp);

      do {
        int childStatus;
        int res = waitpid(pid, &childStatus, 0);

        setProcessRunnable(P);  /* now we can run */

        if (res < 0) {
          switch (errno) {
            case ECHILD:
              return liberror(P, h, "__shell", eNOTFND);
            case EFAULT:
              return liberror(P, h, "__shell", eINVAL);
            case EINTR:
            default:
              continue;
          }
        } else if (WIFEXITED(childStatus)) { /* exited normally */
          return (ReturnStatus) {.ret=Ok,
            .result = (termPo) allocateInteger(h, WEXITSTATUS(childStatus))};
        } else if (WIFSIGNALED(childStatus))
          return liberror(P, h, "__shell", eINTRUPT);
      } while (True);
    }
  }
}

ReturnStatus g__popen(processPo P, heapPo h, termPo a1, termPo a2, termPo a3) {
  switchProcessState(P, wait_io);
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(a1), cmd, NumberOf(cmd));

  termPo args = a2;
  integer argCnt = consLength(args);
  integer envCnt = consLength(a3);

  if (access((char *) cmd, ((unsigned) F_OK) | ((unsigned) R_OK) | ((unsigned) X_OK)) != 0) {
    setProcessRunnable(P);
    return liberror(P, h, "__shell", eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return liberror(P, h, "__shell", eNOPERM);
  } else {
    char **argv = (char **) calloc((size_t) (argCnt + 2), sizeof(char *));
    char **envp = (char **) calloc((size_t) (envCnt + 1), sizeof(char *));
    int pid;

    argv[0] = cmd;
    for (integer ix = 0; ix < argCnt; ix++) {
      char arg[MAXFILELEN];
      copyChars2Buff(C_STR(consHead(C_NORMAL(args))), arg, NumberOf(arg));
      argv[ix + 1] = strdup(arg);
      args = consTail(C_NORMAL(args));
    }

    argv[argCnt + 1] = NULL;
    strBufferPo lineBf = newStringBuffer();
    termPo env = a3;

    for (integer ix = 0; ix < envCnt; ix++) {
      normalPo pair = C_NORMAL(consHead(C_NORMAL(env)));
      env = consTail(C_NORMAL(env));

      integer klen, vlen;
      const char *key = strVal(nthArg(pair, 0), &klen);
      const char *val = strVal(nthArg(pair, 1), &vlen);

      rewindStrBuffer(lineBf);
      outMsg(O_IO(lineBf), "%S = %S", key, klen, val, vlen);

      integer lineLen;
      const char *line = getTextFromBuffer(lineBf, &lineLen);
      envp[ix] = strndup(line, (size_t) lineLen);
    }
    closeFile(O_IO(lineBf));

    envp[envCnt] = NULL;

    ioPo inPipe, outPipe, errPipe;

    switch (openPipe(argv[0], argv, envp, &inPipe, &outPipe, &errPipe, utf8Encoding)) {
      case Ok: {
        ioChnnlPo in = allocateIOChnnl(h, inPipe);
        int root = gcAddRoot(h, (ptrPo) &in);

        ioChnnlPo out = allocateIOChnnl(h, outPipe);
        gcAddRoot(h, (ptrPo) &out);

        ioChnnlPo err = allocateIOChnnl(h, errPipe);
        gcAddRoot(h, (ptrPo) &err);

        setProcessRunnable(P);

        normalPo triple = allocateTpl(h, 3);
        setArg(triple, 0, (termPo) in);
        setArg(triple, 1, (termPo) out);
        setArg(triple, 2, (termPo) err);

        gcReleaseRoot(h, root);

        return (ReturnStatus) {.ret=Ok, .result = (termPo) triple};
      }
      default: {
        for (integer ix = 0; ix < argCnt; ix++)
          free(argv[ix + 1]);    /* release the strings we allocated */
        for (integer ix = 0; ix < envCnt; ix++)
          free(envp[ix]);    /* release the strings we allocated */

        setProcessRunnable(P);
        return liberror(P, h, "__popen", eIOERROR);
      }
    }
  }
}
