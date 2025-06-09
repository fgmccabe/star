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

ReturnStatus g__exit(processPo P) {
  integer ix = integerVal(popVal(P));
  exit((int) ix);
}

static char **argsv = NULL;  /* Store the command line list */
static int argcnt = 0;

void init_args(char **argv, int argc, int start) {
  argsv = &argv[start];
  argcnt = argc - start;
}

termPo commandLine() {
  termPo list = (termPo) nilEnum;
  termPo el = (termPo) voidEnum;
  int root = gcAddRoot(currentHeap, &list);
  gcAddRoot(currentHeap, &el);

  for (integer ix = argcnt - 1; ix >= 0; ix--) {
    el = (termPo) allocateString(currentHeap, argsv[ix], uniStrLen(argsv[ix]));
    list = (termPo) allocateCons(currentHeap, el, list);
  }
  gcReleaseRoot(currentHeap, root);
  return list;
}

extern char **environ;

integer countEnviron() {
  integer ix = 0;
  for (; environ[ix] != Null; ix++);
  return ix;
}

ReturnStatus g__envir(processPo P) {
  integer cnt = countEnviron();
  termPo list = (termPo) nilEnum;
  int root = gcAddRoot(currentHeap, (ptrPo) &list);
  termPo ky = voidEnum;
  termPo vl = voidEnum;
  termPo pair = voidEnum;
  gcAddRoot(currentHeap, &ky);
  gcAddRoot(currentHeap, &vl);
  gcAddRoot(currentHeap, &pair);

  switchProcessState(currentProcess, in_exclusion);

  for (integer ix = 0; ix < cnt; ix++) {
    char *envPair = environ[ix];
    char *pt = strchr(environ[ix], '=');

    if (pt != NULL) {
      ky = (termPo) allocateString(currentHeap, envPair, pt - envPair);
      vl = (termPo) allocateString(currentHeap, pt + 1, uniStrLen(pt + 1));
      pair = (termPo) allocatePair(currentHeap, ky, vl);
      list = (termPo) allocateCons(currentHeap, pair, list);
    }
  }
  gcReleaseRoot(currentHeap, root);
  setProcessRunnable(currentProcess);
  pshVal(P, list);
  return Normal;
}

ReturnStatus g__getenv(processPo P) {
  char key[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(popVal(P)), key, NumberOf(key));

  char *val = getenv((char *) key);

  if (val != NULL)
    pshVal(P, (termPo) wrapSome(currentHeap, allocateCString(currentHeap, val)));
  else
    pshVal(P, noneEnum);
  return Normal;
}

ReturnStatus g__setenv(processPo P) {
  char key[MAX_SYMB_LEN];
  char val[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(popVal(P)), key, NumberOf(key));
  copyChars2Buff(C_STR(popVal(P)), val, NumberOf(val));

  if (setenv((char *) key, val, 1) == 0) {
    pshVal(P, unitEnum);
    return Normal;
  } else {
    pshVal(P, eFAIL);
    return Abnormal;
  }
}

ReturnStatus g__repo(processPo P) {
  char repoBuffer[MAXFILELEN];
  strMsg(repoBuffer, NumberOf(repoBuffer), "%s/", repoDir);
  termPo repo = (termPo) allocateString(currentHeap, repoBuffer, uniStrLen(repoBuffer));

  pshVal(P, repo);
  return Normal;
}

ReturnStatus g__shell(processPo P) {
  switchProcessState(currentProcess, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(popVal(P)), cmd, NumberOf(cmd));

  termPo args = popVal(P);
  termPo env = popVal(P);

  integer argCnt = consLength(args);
  integer envCnt = consLength(env);

  if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
    setProcessRunnable(currentProcess);
    pshVal(P, eNOTFND);
    return Abnormal;
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(currentProcess);
    pshVal(P, eNOPERM);
    return Abnormal;
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

    switchProcessState(currentProcess, wait_child);  /* We are now waiting for a child */

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

        setProcessRunnable(currentProcess);  /* now we can run */

        if (res < 0) {
          switch (errno) {
            case ECHILD:
              pshVal(P, eNOTFND);
              return Abnormal;
            case EFAULT:
              pshVal(P, eINVAL);
              return Abnormal;
            case EINTR:
            default:
              continue;
          }
        } else if (WIFEXITED(childStatus)) { /* exited normally */
          pshVal(P, makeInteger(WEXITSTATUS(childStatus)));
          return Normal;
        } else if (WIFSIGNALED(childStatus)) {
          pshVal(P, eINTRUPT);
          return Abnormal;
        }
      } while (True);
    }
  }
}

ReturnStatus g__popen(processPo P) {
  switchProcessState(currentProcess, wait_io);

  char cmd[MAXFILELEN];

  copyChars2Buff(C_STR(popVal(P)), cmd, NumberOf(cmd));

  termPo args = popVal(P);
  integer argCnt = consLength(args);
  termPo environment = popVal(P);
  integer envCnt = consLength(environment);

  if (access((char *) cmd, ((unsigned) F_OK) | ((unsigned) R_OK) | ((unsigned) X_OK)) != 0) {
    setProcessRunnable(currentProcess);

    pshVal(P, eNOTFND);
    return Abnormal;
  } else if (!isExecutableFile(cmd)) {
    setProcessRunnable(currentProcess);
    pshVal(P, eNOPERM);
    return Abnormal;
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

    ioPo inPipe, outPipe, errPipe;

    switch (openPipe(argv[0], argv, envp, &inPipe, &outPipe, &errPipe, utf8Encoding)) {
      case Ok: {
        ioChnnlPo in = allocateIOChnnl(currentHeap, inPipe);
        int root = gcAddRoot(currentHeap, (ptrPo) &in);

        ioChnnlPo out = allocateIOChnnl(currentHeap, outPipe);
        gcAddRoot(currentHeap, (ptrPo) &out);

        ioChnnlPo err = allocateIOChnnl(currentHeap, errPipe);
        gcAddRoot(currentHeap, (ptrPo) &err);

        setProcessRunnable(currentProcess);

        normalPo triple = allocateTpl(currentHeap, 3);
        setArg(triple, 0, (termPo) in);
        setArg(triple, 1, (termPo) out);
        setArg(triple, 2, (termPo) err);

        gcReleaseRoot(currentHeap, root);
        pshVal(P, (termPo) triple);
        return Normal;
      }
      default: {
        for (integer ix = 0; ix < argCnt; ix++)
          free(argv[ix + 1]);    /* release the strings we allocated */
        for (integer ix = 0; ix < envCnt; ix++)
          free(envp[ix]);    /* release the strings we allocated */

        setProcessRunnable(currentProcess);
        pshVal(P, eIOERROR);
        return Abnormal;
      }
    }
  }
}
