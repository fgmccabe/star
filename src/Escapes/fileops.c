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
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(P));
  pshVal(P, allocateString(processHeap(P), cwBuffer, uniStrLen(cwBuffer)));
  return Normal;
}

ReturnStatus g__cd(processPo P) {
  integer len;
  const char *cd = strVal(popVal(P), &len);

  switch (setProcessWd(P, (char *) cd, len)) {
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (unlink(acFn) != -1) {
    setProcessRunnable(P);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(P);
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rmdir(acFn) == 0) {
    setProcessRunnable(P);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(P);
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t) integerVal(popVal(P));

  tryAgain:
  switchProcessState(P, wait_io);

  if (mkdir(acFn, mode) != -1) {
    setProcessRunnable(P);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(P);
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

  char *srcFn = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char *df = strVal(popVal(P), &dLen);
  char dstBuff[MAXFILELEN];

  char *dstFn = resolveFileName(processWd(P), df, dLen, dstBuff, NumberOf(dstBuff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rename(srcFn, dstFn) != -1) {
    setProcessRunnable(P);
    pshVal(P, unitEnum);
    return Normal;
  } else {
    setProcessRunnable(P);
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

  char *dir = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));

  DIR *directory;

  tryAgain:
  switchProcessState(P, wait_io);

  if ((directory = opendir(dir)) == NULL) {
    setProcessRunnable(P);
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
    termPo list = nilEnum;
    termPo name = voidEnum;
    heapPo h = processHeap(P);
    int root = gcAddRoot(h, &list);
    gcAddRoot(h, &name);

    struct dirent *ent;

    while ((ent = readdir(directory)) != NULL) {
      /* skip special entries "." and ".." */
      if (strcmp(ent->d_name, ".") != 0 && strcmp(ent->d_name, "..") != 0) {
        name = (termPo) allocateString(h, ent->d_name, uniStrLen(ent->d_name));
        list = (termPo) allocateCons(h, name, list);
      }
    }
    closedir(directory);              /* Close the directory stream */

    gcReleaseRoot(h, root);
    setProcessRunnable(P);

    pshVal(P, list);
    return Normal;
  }
}

ReturnStatus g__file_mode(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(P);

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
    setProcessRunnable(P);
    pshVal(P, makeInteger(buf.st_mode));
    return Normal;
  }
}

ReturnStatus g__file_chmod(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t acmode = (mode_t) integerVal(popVal(P));

  tryAgain:
  switchProcessState(P, wait_io);

  if (chmod(acFn, acmode) == -1) {
    setProcessRunnable(P);
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
  setProcessRunnable(P);
  pshVal(P, unitEnum);
  return Normal;
}

ReturnStatus g__file_present(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);
  pshVal(P, present);
  return Normal;
}

ReturnStatus g__isdir(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = isDirectory(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);

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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(P);

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

  setProcessRunnable(P);

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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(P);

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

    setProcessRunnable(P);
    pshVal(P, details);
    return Normal;
  }
}

ReturnStatus g__file_date(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(P);

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
    heapPo h = processHeap(P);
    termPo atime = makeInteger(buf.st_atime);
    int root = gcAddRoot(h, &atime);
    termPo ctime = makeInteger(buf.st_ctime);
    gcAddRoot(h, &ctime);
    termPo mtime = makeInteger(buf.st_mtime);
    gcAddRoot(h, &mtime);
    normalPo triple = allocateTpl(h, 3);

    setArg(triple, 0, atime);
    setArg(triple, 1, ctime);
    setArg(triple, 2, mtime);
    gcReleaseRoot(h, root);

    setProcessRunnable(P);
    pshVal(P, (termPo) triple);
    return Normal;
  }
}

ReturnStatus g__file_modified(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

  tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1) {
    setProcessRunnable(P);

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

    setProcessRunnable(P);
    pshVal(P, mtime);
    return Normal;
  }
}

ReturnStatus g__openInFile(processPo P) {
  integer fnLen;
  const char *fn = strVal(popVal(P), &fnLen);

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);
  heapPo h = processHeap(P);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(h, file));
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(processHeap(P), file));
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(processHeap(P), file));
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

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null) {
    pshVal(P, (termPo) allocateIOChnnl(processHeap(P), file));
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
