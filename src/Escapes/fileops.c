//
// Created by Francis McCabe on 3/7/18.
//

#include <strings.h>
#include <errno.h>
#include <sys/stat.h>
#include <arithP.h>
#include <errorCodes.h>
#include <iochnnlP.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <cons.h>
#include <globals.h>
#include <consP.h>
#include "fileops.h"
#include "tpl.h"

ReturnStatus g__cwd(processPo P) {
  char cwBuffer[MAXFILELEN];
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(currentProcess));
  pshVal(P, allocateString(currentHeap, cwBuffer, uniStrLen(cwBuffer)));
  return Normal;
}

ReturnStatus g__cd(processPo P) {
  integer len;
  const char *cd = strVal(popVal(P), &len);

  switch (setProcessWd(currentProcess, (char *) cd, len)) {
    case Ok:
      pshVal(P, unitEnum);
      return Normal;
    default:
      pshVal(P, eNOPERM);
      return Abnormal;
  }
}

ReturnStatus g__rm(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (unlink(acFn) != -1) {
    setProcessRunnable(currentProcess);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EBUSY:
        pshVal(P, eFAIL);
        return Abnormal;
      case ENOENT:
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

static char *const RMDIR = "__rmdir";

ReturnStatus g__rmdir(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (rmdir(acFn) == 0) {
    setProcessRunnable(currentProcess);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EBUSY:
        pshVal(P, eFAIL);
        return Abnormal;
      case ENOENT:
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

static char *const MKDIR = "__mkdir";

ReturnStatus g__mkdir(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t) integerVal(popVal(P));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (mkdir(acFn, mode) != -1) {
    setProcessRunnable(currentProcess);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EBUSY:
        pshVal(P, eFAIL);
        return Abnormal;
      case ENOENT:
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

static char *const MV = "__mv";

ReturnStatus g__mv(processPo P) {
  integer sLen;
  const char *fn = strVal(popVal(P), &sLen);
  char srcBuff[MAXFILELEN];

  char *srcFn = resolveFileName(processWd(currentProcess), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char *df = strVal(popVal(P), &dLen);
  char dstBuff[MAXFILELEN];

  char *dstFn = resolveFileName(processWd(currentProcess), df, dLen, dstBuff, NumberOf(dstBuff));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (rename(srcFn, dstFn) != -1) {
    setProcessRunnable(currentProcess);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EBUSY:
        pshVal(P, eFAIL);
        return Abnormal;
      case ENOENT:
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

ReturnStatus g__ls(processPo P) {
  integer sLen;
  const char *fn = strVal(popVal(P), &sLen);
  char srcBuff[MAXFILELEN];

  char *dir = resolveFileName(processWd(currentProcess), fn, sLen, srcBuff, NumberOf(srcBuff));

  DIR *directory;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if ((directory = opendir(dir)) == NULL) {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EMFILE:
      case ENFILE:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EBUSY:
        pshVal(P, eFAIL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case ENAMETOOLONG:
      case ENOTDIR:
        pshVal(P, eINVAL);
        return Abnormal;
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  } else {
    termPo list = (termPo) nilEnum;
    termPo name = (termPo) voidEnum;
    int root = gcAddRoot(currentHeap, &list);
    gcAddRoot(currentHeap, &name);

    struct dirent *ent;

    while ((ent = readdir(directory)) != NULL) {
      /* skip special entries "." and ".." */
      if (strcmp(ent->d_name, ".") != 0 && strcmp(ent->d_name, "..") != 0) {
        name = (termPo) allocateString(currentHeap, ent->d_name, uniStrLen(ent->d_name));
        list = (termPo) allocateCons(currentHeap, name, list);
      }
    }
    closedir(directory);              /* Close the directory stream */

    gcReleaseRoot(currentHeap, root);
    setProcessRunnable(currentProcess);

    pshVal(P, list);
    return Normal;
  }
}

ReturnStatus g__file_mode(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(currentProcess);

    switch (errno) {
      case EINTR:
        goto tryAgain;
      case ENOTDIR:
        pshVal(P, eNOFILE);
        return Abnormal;
      case ENAMETOOLONG:
      case ELOOP:
      case EFAULT:
        pshVal(P, eINVAL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case EACCES:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EIO:
        pshVal(P, eIOERROR);
        return Abnormal;
      default:
        pshVal(P, eNOTFND);
        return Abnormal;
    }
  } else {
    setProcessRunnable(currentProcess);
    pshVal(P, makeInteger(buf.st_mode));
    return Normal;
  }
}

ReturnStatus g__file_chmod(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  mode_t acmode = (mode_t) integerVal(popVal(P));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (chmod(acFn, acmode) == -1) {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;    /* A mega hack */
      case EACCES:
      case EPERM:
      default:
        pshVal(P, eNOPERM);
        return Abnormal;
    }
  }
  setProcessRunnable(currentProcess);
  pshVal(P, unitEnum);
  return Normal;
}

ReturnStatus g__file_present(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(currentProcess, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(currentProcess);
  pshVal(P, present);
  return Normal;
}

ReturnStatus g__isdir(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(currentProcess, wait_io);
  termPo present = isDirectory(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(currentProcess);

  pshVal(P, present);
  return Normal;
}

/*
 * file_type check out the type of the file
 */

typedef enum {
  fifoFile = 0,
  directory = 1,
  charfile = 2,
  blockFile = 3,
  plainFile = 4,
  symLink = 5,
  fileSocket = 6
} FileType;

static char *const FILE_DATE = "__file_date";
static char *const FILE_MODIFIED = "__file_modified";

ReturnStatus g__file_type(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(currentProcess);

    switch (errno) {
      case EINTR:
        goto tryAgain;
      case ENOTDIR:
        pshVal(P, eNOFILE);
        return Abnormal;
      case ENAMETOOLONG:
      case ELOOP:
      case EFAULT:
        pshVal(P, eINVAL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case EACCES:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EIO:
        pshVal(P, eIOERROR);
        return Abnormal;
      default:
        pshVal(P, eNOTFND);
        return Abnormal;
    }
  }

  setProcessRunnable(currentProcess);

  termPo type;

  if (S_ISFIFO(buf.st_mode))
    type = makeInteger(fifoFile);
  else if (S_ISCHR(buf.st_mode))
    type = makeInteger(charfile);
  else if (S_ISDIR(buf.st_mode))
    type = makeInteger(directory);
  else if (S_ISBLK(buf.st_mode))
    type = makeInteger(blockFile);
  else if (S_ISREG(buf.st_mode))
    type = makeInteger(plainFile);
  else if (S_ISLNK(buf.st_mode))
    type = makeInteger(symLink);
  else if (S_ISSOCK(buf.st_mode))
    type = makeInteger(fileSocket);
  else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
  pshVal(P, type);
  return Normal;
}

ReturnStatus g__file_size(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(currentProcess);

    switch (errno) {
      case EINTR:
        goto tryAgain;
      case ENOTDIR:
        pshVal(P, eNOFILE);
        return Abnormal;
      case ENAMETOOLONG:
      case ELOOP:
      case EFAULT:
        pshVal(P, eINVAL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case EACCES:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EIO:
        pshVal(P, eIOERROR);
        return Abnormal;
      default:
        pshVal(P, eNOTFND);
        return Abnormal;
    }
  } else {
    termPo details = makeInteger(buf.st_size);

    setProcessRunnable(currentProcess);
    pshVal(P, details);
    return Normal;
  }
}

ReturnStatus g__file_date(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(currentProcess);

    switch (errno) {
      case EINTR:
        goto tryAgain;
      case ENOTDIR:
        pshVal(P, eNOFILE);
        return Abnormal;
      case ENAMETOOLONG:
      case ELOOP:
      case EFAULT:
        pshVal(P, eINVAL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case EACCES:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EIO:
        pshVal(P, eIOERROR);
        return Abnormal;
      default:
        pshVal(P, eNOTFND);
        return Abnormal;
    }
  } else {
    termPo atime = makeInteger(buf.st_atime);
    int root = gcAddRoot(currentHeap, &atime);
    termPo ctime = makeInteger(buf.st_ctime);
    gcAddRoot(currentHeap, &ctime);
    termPo mtime = makeInteger(buf.st_mtime);
    gcAddRoot(currentHeap, &mtime);
    normalPo triple = allocateTpl(currentHeap, 3);

    setArg(triple, 0, atime);
    setArg(triple, 1, ctime);
    setArg(triple, 2, mtime);
    gcReleaseRoot(currentHeap, root);

    setProcessRunnable(currentProcess);
    pshVal(P, (termPo) triple);
    return Normal;
  }
}

ReturnStatus g__file_modified(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(currentProcess);

    switch (errno) {
      case EINTR:
        goto tryAgain;
      case ENOTDIR:
        pshVal(P, eNOFILE);
        return Abnormal;
      case ENAMETOOLONG:
      case ELOOP:
      case EFAULT:
        pshVal(P, eINVAL);
        return Abnormal;
      case ENOENT:
        pshVal(P, eNOTFND);
        return Abnormal;
      case EACCES:
        pshVal(P, eNOPERM);
        return Abnormal;
      case EIO:
        pshVal(P, eIOERROR);
        return Abnormal;
      default:
        pshVal(P, eNOTFND);
        return Abnormal;
    }
  } else {
    termPo mtime = makeInteger(buf.st_mtime);

    setProcessRunnable(currentProcess);
    pshVal(P, mtime);
    return Normal;
  }
}

ReturnStatus g__openInFile(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(currentHeap, file));
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ReturnStatus g__openOutFile(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(currentHeap, file));
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ReturnStatus g__openAppendFile(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(currentHeap, file));
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ReturnStatus g__openAppendIOFile(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(currentHeap, file));
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ioEncoding pickEncoding(integer k) {
  switch (k) {
    case 0:
      return rawEncoding;
    case 3:
      return utf8Encoding;
    default:
      return unknownEncoding;
  }
}
