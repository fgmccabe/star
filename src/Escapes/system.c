/*
 * Some system functions
 */
#include <errno.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <globals.h>
#include <tpl.h>
#include <errorCodes.h>
#include <unistd.h>
#include <arithP.h>
#include <pipe.h>
#include <ioops.h>
#include <iochnnlP.h>
#include <sys/wait.h>
#include "consP.h"
#include "option.h"
#include "stack.h"
#include "escape.h"

// Number of nanos in a second
#define NANOS 1000000000

ValueReturn s__exit(enginePo P, termPo c) {
  integer ix = integerVal(c);
  exit((int) ix);
}

ReturnStatus g__exit(enginePo P) {
  ValueReturn ret = s__exit(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

static char **argsv = NULL; /* Store the command line list */
static int argcnt = 0;

void init_args(char **argv, int argc, int start) {
  argsv = &argv[start];
  argcnt = argc - start;
}

termPo commandLine(heapPo h) {
  termPo list = (termPo) nilEnum;
  termPo el = (termPo) voidEnum;
  int root = gcAddRoot(h, &list);
  gcAddRoot(h, &el);

  for (integer ix = argcnt - 1; ix >= 0; ix--) {
    el = (termPo) allocateString(h, argsv[ix], uniStrLen(argsv[ix]));
    list = (termPo) allocateCons(h, el, list);
  }
  gcReleaseRoot(h, root);
  return list;
}

extern char **environ;

integer countEnviron() {
  integer ix = 0;
  for (; environ[ix] != Null; ix++);
  return ix;
}

ValueReturn s__envir(enginePo P) {
  integer cnt = countEnviron();
  termPo list = nilEnum;

  heapPo h = processHeap(P);
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
      pair = (termPo) allocatePair(h, ky, vl);
      list = (termPo) allocateCons(h, pair, list);
    }
  }
  gcReleaseRoot(h, root);
  setProcessRunnable(P);
  return normalReturn(list);
}

ReturnStatus g__envir(enginePo P) {
  ValueReturn ret = s__envir(P);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__getenv(enginePo P, termPo k) {
  char key[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(k), key, NumberOf(key));

  char *val = getenv((char *) key);

  if (val != NULL) {
    heapPo h = processHeap(P);
    return normalReturn((termPo) wrapSome(h, allocateCString(h, val)));
  } else
    return normalReturn(noneEnum);
}

ReturnStatus g__getenv(enginePo P) {
  termPo k = popVal(P);
  ValueReturn ret = s__getenv(P, k);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__setenv(enginePo P, termPo k, termPo v) {
  char key[MAX_SYMB_LEN];
  char val[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(k), key, NumberOf(key));
  copyChars2Buff(C_STR(v), val, NumberOf(val));

  if (setenv((char *) key, val, 1) == 0) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eFAIL);
  }
}

ReturnStatus g__setenv(enginePo P) {
  termPo k = popVal(P);
  termPo v = popVal(P);

  ValueReturn ret = s__setenv(P, k, v);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__repo(enginePo P) {
  char repoBuffer[MAXFILELEN];
  strMsg(repoBuffer, NumberOf(repoBuffer), "%s/", repoDir);
  return normalReturn(allocateString(processHeap(P), repoBuffer, uniStrLen(repoBuffer)));
}

ReturnStatus g__repo(enginePo P) {
  char repoBuffer[MAXFILELEN];
  strMsg(repoBuffer, NumberOf(repoBuffer), "%s/", repoDir);
  termPo repo = (termPo) allocateString(processHeap(P), repoBuffer, uniStrLen(repoBuffer));

  pshVal(P, repo);
  return Normal;
}

ValueReturn s__shell(enginePo P, termPo c, termPo args, termPo env) {
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(c), cmd, NumberOf(cmd));

  integer argCnt = consLength(args);
  integer envCnt = consLength(env);

  if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
    setProcessRunnable(P);
    return abnormalReturn(eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return abnormalReturn(eNOPERM);
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
      normalPo envTerm = C_NORMAL(env);
      normalPo pair = C_NORMAL(consHead(envTerm));
      env = consTail(envTerm);
      strBufferPo lineBf = newStringBuffer();

      integer klen, vlen;
      const char *key = strVal(nthArg(pair, 0), &klen);
      const char *val = strVal(nthArg(pair, 1), &vlen);

      outMsg(O_IO(lineBf), "%S = %S", key, klen, val, vlen);

      integer lineLen;
      const char *line = getTextFromBuffer(lineBf, &lineLen);
      envp[ix] = strndup(line, (size_t) lineLen);
      closeIo(O_IO(lineBf));
    }

    envp[envCnt] = NULL;

    switchProcessState(P, wait_child); /* We are now waiting for a child */

    if ((pid = fork()) == 0) {
      // child process, terminating after execve
      execve((char *) cmd, argv, envp);
      // abnormal termination -- should never get here
      _exit(127);
    } else {
      // parent process (agent)
      for (integer ix = 1; argv[ix] != NULL; ix++) // argv[0] is a local string
        free(argv[ix]);

      for (integer ix = 0; envp[ix] != NULL; ix++)
        free(envp[ix]);

      free(argv);
      free(envp);

      do {
        int childStatus;
        int res = waitpid(pid, &childStatus, 0);

        setProcessRunnable(P); /* now we can run */

        if (res < 0) {
          switch (errno) {
            case ECHILD:
              return abnormalReturn(eNOTFND);
            case EFAULT:
              return abnormalReturn(eINVAL);
            case EINTR:
            default:
              continue;
          }
        } else if (WIFEXITED(childStatus)) {
          /* exited normally */
          return normalReturn(makeInteger(WEXITSTATUS(childStatus)));
        } else if (WIFSIGNALED(childStatus)) {
          return abnormalReturn(eINTRUPT);
        }
      } while (True);
    }
  }
}

ReturnStatus g__shell(enginePo P) {
  termPo c = popVal(P);
  termPo args = popVal(P);
  termPo env = popVal(P);

  ValueReturn ret = s__shell(P, c, args, env);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__popen(enginePo P, termPo c, termPo args, termPo environment) {
  switchProcessState(P, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(c), cmd, NumberOf(cmd));

  integer argCnt = consLength(args);
  integer envCnt = consLength(environment);

  if (access((char *) cmd, ((unsigned) F_OK) | ((unsigned) R_OK) | ((unsigned) X_OK)) != 0) {
    setProcessRunnable(P);

    return abnormalReturn(eNOTFND);
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(P);
    return abnormalReturn(eNOPERM);
  } else {
    char **argv = (char **) calloc((size_t) (argCnt + 2), sizeof(char *));
    char **envp = (char **) calloc((size_t) (envCnt + 1), sizeof(char *));

    argv[0] = cmd;
    for (integer ix = 0; ix < argCnt; ix++) {
      char arg[MAXFILELEN];
      copyChars2Buff(C_STR(consHead(C_NORMAL(args))), arg, NumberOf(arg));
      argv[ix + 1] = strdup(arg);
      args = consTail(C_NORMAL(args));
    }

    argv[argCnt + 1] = NULL;
    strBufferPo lineBf = newStringBuffer();
    termPo env = environment;

    for (integer ix = 0; ix < envCnt; ix++) {
      normalPo envTerm = C_NORMAL(env);
      normalPo pair = C_NORMAL(consHead(envTerm));
      env = consTail(envTerm);

      integer klen, vlen;
      const char *key = strVal(nthArg(pair, 0), &klen);
      const char *val = strVal(nthArg(pair, 1), &vlen);

      rewindStrBuffer(lineBf);
      outMsg(O_IO(lineBf), "%S = %S", key, klen, val, vlen);

      integer lineLen;
      const char *line = getTextFromBuffer(lineBf, &lineLen);
      envp[ix] = strndup(line, (size_t) lineLen);
    }
    closeIo(O_IO(lineBf));

    envp[envCnt] = NULL;
    heapPo h = processHeap(P);
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
        return normalReturn((termPo)triple);
      }
      default: {
        for (integer ix = 0; ix < argCnt; ix++)
          free(argv[ix + 1]); /* release the strings we allocated */
        for (integer ix = 0; ix < envCnt; ix++)
          free(envp[ix]); /* release the strings we allocated */

        setProcessRunnable(P);
        return abnormalReturn(eIOERROR);
      }
    }
  }
}

ReturnStatus g__popen(enginePo P) {
  termPo c = popVal(P);
  termPo args = popVal(P);
  termPo env = popVal(P);

  ValueReturn ret = s__popen(P, c, args, env);
  pshVal(P, ret.value);
  return ret.status;
}
