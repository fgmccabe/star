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

ReturnStatus g__cwd(heapPo h) {
  char cwBuffer[MAXFILELEN];
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(currentProcess));
  termPo cwd = (termPo) allocateString(h, cwBuffer, uniStrLen(cwBuffer));

  return (ReturnStatus) {.ret=Normal, .result = cwd};
}

ReturnStatus g__cd(processPo p, termPo a1) {
  integer len;
  const char *cd = strVal(a1, &len);

  switch (setProcessWd(p, (char *) cd, len)) {
    case Ok:
      return (ReturnStatus) {.ret=Normal, .result = unitEnum};
    default:
      return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eRANGE};
  }
}

ReturnStatus g__rm(processPo p, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(p), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(p, wait_io);

  if (unlink(acFn) != -1) {
    setProcessRunnable(p);
    return (ReturnStatus) {.ret=Normal, .result = unitEnum};
  } else {
    setProcessRunnable(p);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOPERM};
      case EBUSY:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eFAIL};
      case ENOENT:
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eIOERROR};
    }
  }
}

static char *const RMDIR = "__rmdir";

ReturnStatus g__rmdir(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (rmdir(acFn) == 0) {
    setProcessRunnable(currentProcess);
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOPERM};
      case EBUSY:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eFAIL};
      case ENOENT:
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eIOERROR};
    }
  }
}

static char *const MKDIR = "__mkdir";

ReturnStatus g__mkdir(heapPo h, termPo a1, termPo a2) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t) integerVal(a2);

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (mkdir(acFn, mode) != -1) {
    setProcessRunnable(currentProcess);
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOPERM};
      case EBUSY:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eFAIL};
      case ENOENT:
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eIOERROR};
    }
  }
}

static char *const MV = "__mv";

ReturnStatus g__mv(heapPo h, termPo a1, termPo a2) {
  integer sLen;
  const char *fn = strVal(a1, &sLen);
  char srcBuff[MAXFILELEN];

  char *srcFn = resolveFileName(processWd(currentProcess), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char *df = strVal(a2, &dLen);
  char dstBuff[MAXFILELEN];

  char *dstFn = resolveFileName(processWd(currentProcess), df, dLen, dstBuff, NumberOf(dstBuff));

  tryAgain:
  switchProcessState(currentProcess, wait_io);

  if (rename(srcFn, dstFn) != -1) {
    setProcessRunnable(currentProcess);
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  } else {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOPERM};
      case EBUSY:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eFAIL};
      case ENOENT:
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eIOERROR};
    }
  }
}

ReturnStatus g__ls(heapPo h, termPo a1) {
  integer sLen;
  const char *fn = strVal(a1, &sLen);
  char srcBuff[MAXFILELEN];

  char *dir = resolveFileName(processWd(currentProcess), fn, sLen, srcBuff, NumberOf(srcBuff));

  DIR *directory;

  switchProcessState(currentProcess, wait_io);

  if ((directory = opendir(dir)) == NULL) {
    setProcessRunnable(currentProcess);
    switch (errno) {
      case EACCES:
      case EMFILE:
      case ENFILE:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case ENAMETOOLONG:
      case ENOTDIR:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
    }
  } else {
    termPo list = (termPo) nilEnum;
    termPo name = (termPo) voidEnum;
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
    setProcessRunnable(currentProcess);

    return (ReturnStatus) {.ret=Normal, .result = (termPo) list};
  }
}

ReturnStatus g__file_mode(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOFILE};
      case ENAMETOOLONG:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case EACCES:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ELOOP:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case EIO:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eIOERROR};
      case EFAULT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
    }
  } else {
    setProcessRunnable(currentProcess);

    return (ReturnStatus) {.ret=Normal, .result = makeInteger(buf.st_mode)};
  }
}

ReturnStatus g__file_chmod(heapPo h, termPo a1, termPo a2) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  mode_t acmode = (mode_t) integerVal(a2);

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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOPERM};
    }
  }
  setProcessRunnable(currentProcess);

  return (ReturnStatus) {.ret=Normal, .result = unitEnum};
}

ReturnStatus g__file_present(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(currentProcess, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(currentProcess);

  return (ReturnStatus) {.ret=Normal, .result = present};
}

ReturnStatus g__isdir(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(currentProcess, wait_io);
  retCode present = isDirectory(acFn);
  setProcessRunnable(currentProcess);

  return (ReturnStatus) {.ret=Normal, .result = present == Ok ? trueEnum : falseEnum};
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

ReturnStatus g__file_type(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOFILE};
      case ENAMETOOLONG:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case EACCES:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ELOOP:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case EIO:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eIOERROR};
      case EFAULT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
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
  else
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};

  return (ReturnStatus) {.ret=Normal, .result =type};
}

ReturnStatus g__file_size(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTDIR};
      case ENAMETOOLONG:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case EACCES:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ELOOP:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case EIO:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eIOERROR};
      case EFAULT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
    }
  } else {
    termPo details = makeInteger(buf.st_size);

    setProcessRunnable(currentProcess);

    return (ReturnStatus) {.ret=Normal, .result =details};
  }
}

ReturnStatus g__file_date(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTDIR};
      case ENAMETOOLONG:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case EACCES:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ELOOP:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case EIO:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eIOERROR};
      case EFAULT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
    }
  } else {
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

    setProcessRunnable(currentProcess);

    return (ReturnStatus) {.ret=Normal, .result =(termPo) triple};
  }
}

ReturnStatus g__file_modified(heapPo h, termPo a1) {
  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
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
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTDIR};
      case ENAMETOOLONG:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case ENOENT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
      case EACCES:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOPERM};
      case ELOOP:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      case EIO:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eIOERROR};
      case EFAULT:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eINVAL};
      default:
        return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
    }
  } else {
    termPo mtime = makeInteger(buf.st_mtime);

    setProcessRunnable(currentProcess);

    return (ReturnStatus) {.ret=Normal, .result =mtime};
  }
}

ReturnStatus g__openInFile(heapPo h, termPo a1, termPo a2) {
  ioEncoding enc = pickEncoding(integerVal(a2));

  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Normal,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOTFND};
}

ReturnStatus g__openOutFile(heapPo h, termPo a1, termPo a2) {
  ioEncoding enc = pickEncoding(integerVal(a2));

  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Normal,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOTFND};
}

ReturnStatus g__openAppendFile(heapPo h, termPo a1, termPo a2) {
  ioEncoding enc = pickEncoding(integerVal(a2));

  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Normal,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result = eNOTFND};
}

ReturnStatus g__openAppendIOFile(heapPo h, termPo a1, termPo a2) {
  ioEncoding enc = pickEncoding(integerVal(a2));

  integer fnLen;
  const char *fn = strVal(a1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(currentProcess), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Normal,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result =eNOTFND};
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
