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

ReturnStatus g__cwd(processPo p, heapPo h, ptrPo tos) {
  char cwBuffer[MAXFILELEN];
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(p));
  termPo cwd = (termPo) allocateString(h, cwBuffer, uniStrLen(cwBuffer));

  return (ReturnStatus) {.result = cwd, .ret=Ok};
}

ReturnStatus g__cd(processPo p, heapPo h, ptrPo tos) {
  integer len;
  const char *cd = strVal(tos[0], &len);

  return rtnStatus(p, h, setProcessWd(p, (char *) cd, len), "cd problem");
}

ReturnStatus g__rm(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (unlink(acFn) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, h, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, h, "__rm", eNOPERM);
      case EBUSY:
        return liberror(P, h, "__rm", eFAIL);
      case ENOENT:
      default:
        return liberror(P, h, "__rm", eIOERROR);
    }
  }
}

static char *const RMDIR = "__rmdir";

ReturnStatus g__rmdir(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rmdir(acFn) == 0) {
    setProcessRunnable(P);
    return rtnStatus(P, h, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, h, RMDIR, eNOPERM);
      case EBUSY:
        return liberror(P, h, RMDIR, eFAIL);
      case ENOENT:
      default:
        return liberror(P, h, RMDIR, eIOERROR);
    }
  }
}

static char *const MKDIR = "__mkdir";

ReturnStatus g__mkdir(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];

  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t) integerVal(Arg2);

  tryAgain:
  switchProcessState(P, wait_io);

  if (mkdir(acFn, mode) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, h, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, h, MKDIR, eNOPERM);
      case EBUSY:
        return liberror(P, h, MKDIR, eFAIL);
      case ENOENT:
      default:
        return liberror(P, h, MKDIR, eIOERROR);
    }
  }
}

static char *const MV = "__mv";

ReturnStatus g__mv(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer sLen;
  const char *fn = strVal(Arg1, &sLen);
  char srcBuff[MAXFILELEN];

  char *srcFn = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char *df = strVal(Arg2, &dLen);
  char dstBuff[MAXFILELEN];

  char *dstFn = resolveFileName(processWd(P), fn, sLen, dstBuff, NumberOf(dstBuff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rename(srcFn, dstFn) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, h, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, h, MV, eNOPERM);
      case EBUSY:
        return liberror(P, h, MV, eFAIL);
      case ENOENT:
      default:
        return liberror(P, h, MV, eIOERROR);
    }
  }
}

ReturnStatus g__ls(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer sLen;
  const char *fn = strVal(Arg1, &sLen);
  char srcBuff[MAXFILELEN];

  char *dir = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));

  DIR *directory;

  switchProcessState(P, wait_io);

  if ((directory = opendir(dir)) == NULL) {
    setProcessRunnable(P);
    switch (errno) {
      case EACCES:
      case EMFILE:
      case ENFILE:
        return liberror(P, h, "__ls", eNOPERM);
      case ENOENT:
        return liberror(P, h, "__ls", eNOTFND);
      case ENAMETOOLONG:
      case ENOTDIR:
        return liberror(P, h, "__ls", eINVAL);
      default:
        return liberror(P, h, "__ls", eNOTFND);
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
    setProcessRunnable(P);

    ReturnStatus ret = {.ret=Ok, .result = (termPo) list};
    return ret;
  }
}

static char *const FILE_MODE = "_file_mode";

ReturnStatus g__file_mode(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
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
        return liberror(P, h, FILE_MODE, eNOFILE);
      case ENAMETOOLONG:
        return liberror(P, h, FILE_MODE, eINVAL);
      case ENOENT:
        return liberror(P, h, FILE_MODE, eNOTFND);
      case EACCES:
        return liberror(P, h, FILE_MODE, eNOPERM);
      case ELOOP:
        return liberror(P, h, FILE_MODE, eINVAL);
      case EIO:
        return liberror(P, h, FILE_MODE, eIOERROR);
      case EFAULT:
        return liberror(P, h, FILE_MODE, eINVAL);
      default:
        return liberror(P, h, FILE_MODE, eNOTFND);
    }
  } else {
    ReturnStatus ret = {.ret=Ok, .result = (termPo) allocateInteger(h, buf.st_mode)};

    setProcessRunnable(P);
    return ret;
  }
}

ReturnStatus g__file_chmod(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t acmode = (mode_t) integerVal(Arg2);

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
        return liberror(P, h, "__file_chmod", eNOPERM);
    }
  }
  setProcessRunnable(P);

  return rtnStatus(P, h, Ok, "");
}

ReturnStatus g__file_present(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);

  ReturnStatus ret = {.ret=Ok, .result = present};

  return ret;
}

ReturnStatus g__isdir(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  retCode present = isDirectory(acFn);
  setProcessRunnable(P);

  return (ReturnStatus) {.ret=Ok, .result = present == Ok ? trueEnum : falseEnum};
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

ReturnStatus g__file_type(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
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
        return liberror(P, h, "__file_type", eNOFILE);
      case ENAMETOOLONG:
        return liberror(P, h, "__file_type", eINVAL);
      case ENOENT:
        return liberror(P, h, "__file_type", eNOTFND);
      case EACCES:
        return liberror(P, h, "__file_type", eNOPERM);
      case ELOOP:
        return liberror(P, h, "__file_type", eINVAL);
      case EIO:
        return liberror(P, h, "__file_type", eIOERROR);
      case EFAULT:
        return liberror(P, h, "__file_type", eINVAL);
      default:
        return liberror(P, h, "__file_type", eNOTFND);
    }
  }

  setProcessRunnable(P);

  termPo type;

  if (S_ISFIFO(buf.st_mode))
    type = (termPo) allocateInteger(h, fifoFile);
  else if (S_ISCHR(buf.st_mode))
    type = (termPo) allocateInteger(h, charfile);
  else if (S_ISDIR(buf.st_mode))
    type = (termPo) allocateInteger(h, directory);
  else if (S_ISBLK(buf.st_mode))
    type = (termPo) allocateInteger(h, blockFile);
  else if (S_ISREG(buf.st_mode))
    type = (termPo) allocateInteger(h, plainFile);
  else if (S_ISLNK(buf.st_mode))
    type = (termPo) allocateInteger(h, symLink);
  else if (S_ISSOCK(buf.st_mode))
    type = (termPo) allocateInteger(h, fileSocket);
  else
    return liberror(P, h, "__file_type", eINVAL);

  return (ReturnStatus) {.ret=Ok, .result =type};
}

ReturnStatus g__file_size(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
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
        return liberror(P, h, "__file_size", eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, h, "__file_size", eINVAL);
      case ENOENT:
        return liberror(P, h, "__file_size", eNOTFND);
      case EACCES:
        return liberror(P, h, "__file_size", eNOPERM);
      case ELOOP:
        return liberror(P, h, "__file_size", eINVAL);
      case EIO:
        return liberror(P, h, "__file_size", eIOERROR);
      case EFAULT:
        return liberror(P, h, "__file_size", eINVAL);
      default:
        return liberror(P, h, "__file_size", eNOTFND);
    }
  } else {
    termPo details = (termPo) allocateInteger(h, buf.st_size);

    setProcessRunnable(P);

    return (ReturnStatus) {.ret=Ok, .result =details};
  }
}

ReturnStatus g__file_date(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
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
        return liberror(P, h, FILE_DATE, eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, h, FILE_DATE, eINVAL);
      case ENOENT:
        return liberror(P, h, FILE_DATE, eNOTFND);
      case EACCES:
        return liberror(P, h, FILE_DATE, eNOPERM);
      case ELOOP:
        return liberror(P, h, FILE_DATE, eINVAL);
      case EIO:
        return liberror(P, h, FILE_DATE, eIOERROR);
      case EFAULT:
        return liberror(P, h, FILE_DATE, eINVAL);
      default:
        return liberror(P, h, FILE_DATE, eNOTFND);
    }
  } else {
    termPo atime = (termPo) allocateInteger(h, buf.st_atime);
    int root = gcAddRoot(h, &atime);
    termPo ctime = (termPo) allocateInteger(h, buf.st_ctime);
    gcAddRoot(h, &ctime);
    termPo mtime = (termPo) allocateInteger(h, buf.st_mtime);
    gcAddRoot(h, &mtime);
    normalPo triple = allocateTpl(h, 3);

    setArg(triple, 0, atime);
    setArg(triple, 1, ctime);
    setArg(triple, 2, mtime);
    gcReleaseRoot(h, root);

    setProcessRunnable(P);

    return (ReturnStatus) {.ret=Ok, .result =(termPo) triple};
  }
}

ReturnStatus g__file_modified(processPo P, heapPo h, ptrPo tos) {
  integer fnLen;
  const char *fn = strVal(tos[0], &fnLen);
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
        return liberror(P, h, FILE_MODIFIED, eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, h, FILE_MODIFIED, eINVAL);
      case ENOENT:
        return liberror(P, h, FILE_MODIFIED, eNOTFND);
      case EACCES:
        return liberror(P, h, FILE_MODIFIED, eNOPERM);
      case ELOOP:
        return liberror(P, h, FILE_MODIFIED, eINVAL);
      case EIO:
        return liberror(P, h, FILE_MODIFIED, eIOERROR);
      case EFAULT:
        return liberror(P, h, FILE_MODIFIED, eINVAL);
      default:
        return liberror(P, h, FILE_MODIFIED, eNOTFND);
    }
  } else {
    termPo mtime = (termPo) allocateInteger(h, buf.st_mtime);

    setProcessRunnable(P);

    return (ReturnStatus) {.ret=Ok, .result =mtime};
  }
}

ReturnStatus g__openInFile(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Ok,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return liberror(P, h, "_openInFile", eNOTFND);
}

ReturnStatus g__openOutFile(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Ok,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return liberror(P, h, "_openOutFile", eNOTFND);
}

ReturnStatus g__openAppendFile(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Ok,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return liberror(P, h, "_openAppendFile", eNOTFND);
}

ReturnStatus g__openAppendIOFile(processPo P, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = strVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null) {
    return (ReturnStatus) {.ret=Ok,
      .result =(termPo) allocateIOChnnl(h, file)};
  } else
    return liberror(P, h, "_openAppendIOFile", eNOTFND);
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
