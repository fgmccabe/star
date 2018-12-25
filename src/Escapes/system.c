/*
 * Some system functions
 */
#include <sys/time.h>
#include <errno.h>
#include <stdlib.h>
#include <array.h>
#include <str.h>
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

ReturnStatus g__exit(processPo p, ptrPo tos) {
  integer ix = integerVal(tos[0]);

  exit((int) ix);
}

ReturnStatus g__assert(processPo P, ptrPo tos) {
  if (tos[0] != trueEnum) {
    logMsg(logFile, "assertion failed at %L\n", tos[1]);
    exit(10);
  }
  ReturnStatus rt = {.ret=Ok, .result=unitEnum};
  return rt;
}

static char **argsv = NULL;  /* Store the command line list */
static int argcnt = 0;

void init_args(char **argv, int argc, int start) {
  argsv = &argv[start];
  argcnt = argc - start;
}

ReturnStatus g__command_line(processPo p, ptrPo tos) {
  heapPo H = processHeap(p);
  listPo line = allocateList(H, argcnt, True);
  int root = gcAddRoot(H, (ptrPo) &line);

  for (integer ix = 0; ix < argcnt; ix++) {
    stringPo arg = allocateString(H, argsv[ix], uniStrLen(argsv[ix]));
    setNthEl(line, ix, (termPo) arg);
  }

  gcReleaseRoot(H, root);
  ReturnStatus rt = {.ret=Ok, .result=(termPo) line};
  return rt;
}

extern char **environ;

integer countEnviron() {
  integer ix = 0;
  for (; environ[ix] != Null; ix++);
  return ix;
}

ReturnStatus g__envir(processPo P, ptrPo tos) {
  integer cnt = countEnviron();
  heapPo H = processHeap(P);
  listPo list = allocateList(H, cnt, True);
  int root = gcAddRoot(H, (ptrPo) &list);
  termPo ky = voidEnum;
  termPo vl = voidEnum;
  gcAddRoot(H, &ky);
  gcAddRoot(H, &vl);

  switchProcessState(P, in_exclusion);

  for (integer ix = 0; ix < cnt; ix++) {
    char *envPair = environ[ix];
    char *pt = strchr(environ[ix], '=');

    if (pt != NULL) {
      ky = (termPo) allocateString(H, envPair, pt - envPair);
      vl = (termPo) allocateString(H, pt + 1, uniStrLen(pt + 1));
    } else {
      ky = (termPo) allocateCString(H, envPair);
      vl = voidEnum;
    }
    normalPo pair = allocatePair(H, ky, vl);
    setNthEl(list, ix, (termPo) pair);
  }
  gcReleaseRoot(NULL, root);
  setProcessRunnable(P);
  ReturnStatus rt = {.ret=Ok, .result=(termPo) list};
  return rt;
}

ReturnStatus g__getenv(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char key[MAX_SYMB_LEN];

  copyString2Buff(C_STR(Arg1), key, NumberOf(key));

  char *val = getenv((char *) key);

  if (val != NULL) {
    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateCString(processHeap(P), val)};
    return rt;
  } else {
    ReturnStatus rt = {.ret=Ok, .result=Arg2};
    return rt;
  }
}

ReturnStatus g__setenv(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char key[MAX_SYMB_LEN];
  char val[MAX_SYMB_LEN];

  copyString2Buff(C_STR(Arg1), key, NumberOf(key));
  copyString2Buff(C_STR(Arg2), val, NumberOf(val));

  if (setenv((char *) key, val, 1) == 0) {
    ReturnStatus rt = {.ret=Ok, .result=voidEnum};
    return rt;
  } else
    return liberror(P, "_setenv", eFAIL);
}

ReturnStatus g__repo(processPo p, ptrPo tos) {
  char repoBuffer[MAXFILELEN];
  strMsg(repoBuffer, NumberOf(repoBuffer), "%s/", repoDir);
  termPo repo = (termPo) allocateString(processHeap(p), repoBuffer, uniStrLen(repoBuffer));

  ReturnStatus rtn = {.result = repo, .ret=Ok};
  return rtn;
}

ReturnStatus g__getlogin(processPo P, ptrPo tos) {
  ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateCString(processHeap(P), getlogin())};
  return rt;
}

ReturnStatus g__shell(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyString2Buff(C_STR(Arg1), cmd, NumberOf(cmd));

  listPo args = C_LIST(Arg2);
  listPo env = C_LIST(Arg3);

  integer argCnt = listSize(args);
  integer envCnt = listSize(env);

  if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
    setProcessRunnable(P);
    return liberror(P, "__shell", eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return liberror(P, "__shell", eNOPERM);
  } else {
    char **argv = (char **) calloc((size_t) (argCnt + 2), sizeof(char *));
    char **envp = (char **) calloc((size_t) (listSize(env) + 1), sizeof(char *));
    int pid;

    argv[0] = cmd;

    for (integer ix = 0; ix < argCnt; ix++) {
      char arg[MAXFILELEN];
      copyString2Buff(C_STR(nthEl(args, ix)), arg, NumberOf(arg));
      argv[ix + 1] = strdup(arg);
    }

    argv[argCnt + 1] = NULL;

    for (integer ix = 0; ix < envCnt; ix++) {
      normalPo pair = C_TERM(nthEl(env, ix));

      bufferPo lineBf = newStringBuffer();

      integer klen, vlen;
      const char *key = stringVal(nthArg(pair, 0), &klen);
      const char *val = stringVal(nthArg(pair, 1), &vlen);

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
              return liberror(P, "__shell", eNOTFND);
            case EFAULT:
              return liberror(P, "__shell", eINVAL);
            case EINTR:
            default:
              continue;
          }
        } else if (WIFEXITED(childStatus)) { /* exited normally */
          ReturnStatus rt = {.ret=Ok, .result = (termPo) allocateInteger(processHeap(P), WEXITSTATUS(childStatus))};
          return rt;
        } else if (WIFSIGNALED(childStatus))
          return liberror(P, "__shell", eINTRUPT);
      } while (True);
    }
  }
}

ReturnStatus g__popen(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  switchProcessState(P, wait_io);
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyString2Buff(C_STR(Arg1), cmd, NumberOf(cmd));

  listPo args = C_LIST(Arg2);
  listPo env = C_LIST(Arg3);

  integer argCnt = listSize(args);
  integer envCnt = listSize(env);

  if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
    setProcessRunnable(P);
    return liberror(P, "__shell", eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return liberror(P, "__shell", eNOPERM);
  } else {
    char **argv = (char **) calloc((size_t) (argCnt + 2), sizeof(char *));
    char **envp = (char **) calloc((size_t) (listSize(env) + 1), sizeof(char *));
    int pid;

    argv[0] = cmd;

    for (integer ix = 0; ix < argCnt; ix++) {
      char arg[MAXFILELEN];
      copyString2Buff(C_STR(nthEl(args, ix)), arg, NumberOf(arg));
      argv[ix + 1] = strdup(arg);
    }

    argv[argCnt + 1] = NULL;

    for (integer ix = 0; ix < envCnt; ix++) {
      normalPo pair = C_TERM(nthEl(env, ix));

      bufferPo lineBf = newStringBuffer();

      integer klen, vlen;
      const char *key = stringVal(nthArg(pair, 0), &klen);
      const char *val = stringVal(nthArg(pair, 1), &vlen);

      outMsg(O_IO(lineBf), "%S = %S", key, klen, val, vlen);

      integer lineLen;
      const char *line = getTextFromBuffer(lineBf, &lineLen);
      envp[ix] = strndup(line, (size_t) lineLen);
      closeFile(O_IO(lineBf));
    }

    envp[envCnt] = NULL;

    ioPo inPipe, outPipe, errPipe;
    heapPo H = processHeap(P);

    switch (openPipe(argv[0], argv, envp, &inPipe, &outPipe, &errPipe, utf8Encoding)) {
      case Ok: {
        ioChnnlPo in = allocateIOChnnl(H, inPipe);
        int root = gcAddRoot(H, (ptrPo) &in);

        ioChnnlPo out = allocateIOChnnl(H, outPipe);
        gcAddRoot(H, (ptrPo) &out);

        ioChnnlPo err = allocateIOChnnl(H, errPipe);
        gcAddRoot(H, (ptrPo) &err);

        setProcessRunnable(P);

        normalPo triple = allocateTpl(H, 3);
        setArg(triple, 0, (termPo) in);
        setArg(triple, 1, (termPo) out);
        setArg(triple, 2, (termPo) err);

        gcReleaseRoot(H, root);

        ReturnStatus rt = {.ret=Ok, .result = (termPo) triple};
        return rt;
      }
      default: {
        for (integer ix = 0; ix < argCnt; ix++)
          free(argv[ix + 1]);    /* release the strings we allocated */
        for (integer ix = 0; ix < envCnt; ix++)
          free(envp[ix]);    /* release the strings we allocated */

        setProcessRunnable(P);
        return liberror(P, "__popen", eIOERROR);
      }
    }
  }
}
