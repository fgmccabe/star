//
// Created by Francis McCabe on 3/7/18.
//

#include <str.h>
#include <errno.h>
#include <sys/stat.h>
#include <arithP.h>
#include <errorCodes.h>
#include <escodes.h>
#include <iochnnlP.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <array.h>
#include "fileops.h"
#include "tpl.h"

ReturnStatus g__cwd(processPo p, ptrPo tos) {
  char cwBuffer[MAXFILELEN];
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(p));
  termPo cwd = (termPo) allocateString(processHeap(p), cwBuffer, uniStrLen(cwBuffer));

  ReturnStatus rtn = {.result = cwd, .ret=Ok};
  return rtn;
}

ReturnStatus g__cd(processPo p, ptrPo tos) {
  integer len;
  const char *cd = stringVal(tos[0], &len);

  return rtnStatus(p, setProcessWd(p, (char *) cd, len), "cd problem");
}

ReturnStatus g__rm(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (unlink(acFn) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, "__rm", eNOPERM);
      case EBUSY:
        return liberror(P, "__rm", eFAIL);
      case ENOENT:
      default:
        return liberror(P, "__rm", eIOERROR);
    }
  }
}

static char *const RMDIR = "__rmdir";

ReturnStatus g__rmdir(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rmdir(acFn) == 0) {
    setProcessRunnable(P);
    return rtnStatus(P, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, RMDIR, eNOPERM);
      case EBUSY:
        return liberror(P, RMDIR, eFAIL);
      case ENOENT:
      default:
        return liberror(P, RMDIR, eIOERROR);
    }
  }
}

static char *const MKDIR = "__mkdir";

ReturnStatus g__mkdir(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];

  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t) integerVal(Arg2);

  tryAgain:
  switchProcessState(P, wait_io);

  if (mkdir(acFn, mode) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, MKDIR, eNOPERM);
      case EBUSY:
        return liberror(P, MKDIR, eFAIL);
      case ENOENT:
      default:
        return liberror(P, MKDIR, eIOERROR);
    }
  }
}

static char *const MV = "__mv";

ReturnStatus g__mv(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer sLen;
  const char *fn = stringVal(Arg1, &sLen);
  char srcBuff[MAXFILELEN];

  char *srcFn = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char *df = stringVal(Arg2, &dLen);
  char dstBuff[MAXFILELEN];

  char *dstFn = resolveFileName(processWd(P), fn, sLen, dstBuff, NumberOf(dstBuff));

  tryAgain:
  switchProcessState(P, wait_io);

  if (rename(srcFn, dstFn) != -1) {
    setProcessRunnable(P);
    return rtnStatus(P, Ok, "");
  } else {
    setProcessRunnable(P);
    switch (errno) {
      case EINTR:
        goto tryAgain;
      case EACCES:
      case EPERM:
        return liberror(P, MV, eNOPERM);
      case EBUSY:
        return liberror(P, MV, eFAIL);
      case ENOENT:
      default:
        return liberror(P, MV, eIOERROR);
    }
  }
}

ReturnStatus g__ls(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer sLen;
  const char *fn = stringVal(Arg1, &sLen);
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
        return liberror(P, "__ls", eNOPERM);
      case ENOENT:
        return liberror(P, "__ls", eNOTFND);
      case ENAMETOOLONG:
      case ENOTDIR:
        return liberror(P, "__ls", eINVAL);
      default:
        return liberror(P, "__ls", eNOTFND);
    }
  } else {
    heapPo H = processHeap(P);
    listPo list = allocateList(H, 8, True);
    int root = gcAddRoot(H, (ptrPo) &list);

    struct dirent *ent;

    while ((ent = readdir(directory)) != NULL) {
      /* skip special entries "." and ".." */
      if (strcmp(ent->d_name, ".") != 0 && strcmp(ent->d_name, "..") != 0) {
        termPo name = (termPo) allocateString(H, ent->d_name, uniStrLen(ent->d_name));
        list = appendToList(H, list, name);
      }
    }
    closedir(directory);              /* Close the directory stream */

    gcReleaseRoot(H, root);
    setProcessRunnable(P);

    ReturnStatus ret = {.ret=Ok, .result = (termPo) list};
    return ret;
  }
}

static char *const FILE_MODE = "_file_mode";

ReturnStatus g__file_mode(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
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
        return liberror(P, FILE_MODE, eNOFILE);
      case ENAMETOOLONG:
        return liberror(P, FILE_MODE, eINVAL);
      case ENOENT:
        return liberror(P, FILE_MODE, eNOTFND);
      case EACCES:
        return liberror(P, FILE_MODE, eNOPERM);
      case ELOOP:
        return liberror(P, FILE_MODE, eINVAL);
      case EIO:
        return liberror(P, FILE_MODE, eIOERROR);
      case EFAULT:
        return liberror(P, FILE_MODE, eINVAL);
      default:
        return liberror(P, FILE_MODE, eNOTFND);
    }
  } else {
    ReturnStatus ret = {.ret=Ok, .result = (termPo) allocateInteger(processHeap(P), buf.st_mode)};

    setProcessRunnable(P);
    return ret;
  }
}

ReturnStatus g__file_chmod(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
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
        return liberror(P, "__file_chmod", eNOPERM);
      case EPERM:
        return liberror(P, "__file_chmod", eNOPERM);
      default:
        return liberror(P, "__file_chmod", eNOPERM);
    }
  }
  setProcessRunnable(P);

  return rtnStatus(P, Ok, "");
}

ReturnStatus g__file_present(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);

  ReturnStatus ret = {.ret=Ok, .result = present};

  return ret;
}

ReturnStatus g__isdir(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  retCode present = isDirectory(acFn);
  setProcessRunnable(P);

  ReturnStatus ret = {.ret=Ok, .result = present == Ok ? trueEnum : falseEnum};

  return ret;
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

ReturnStatus g__file_type(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
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
        return liberror(P, "__file_type", eNOFILE);
      case ENAMETOOLONG:
        return liberror(P, "__file_type", eINVAL);
      case ENOENT:
        return liberror(P, "__file_type", eNOTFND);
      case EACCES:
        return liberror(P, "__file_type", eNOPERM);
      case ELOOP:
        return liberror(P, "__file_type", eINVAL);
      case EIO:
        return liberror(P, "__file_type", eIOERROR);
      case EFAULT:
        return liberror(P, "__file_type", eINVAL);
      default:
        return liberror(P, "__file_type", eNOTFND);
    }
  }

  setProcessRunnable(P);

  termPo type;

  if (S_ISFIFO(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), fifoFile);
  else if (S_ISCHR(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), charfile);
  else if (S_ISDIR(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), directory);
  else if (S_ISBLK(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), blockFile);
  else if (S_ISREG(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), plainFile);
  else if (S_ISLNK(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), symLink);
  else if (S_ISSOCK(buf.st_mode))
    type = (termPo) allocateInteger(processHeap(P), fileSocket);
  else
    return liberror(P, "__file_type", eINVAL);

  ReturnStatus ret = {.ret=Ok, .result =type};

  return ret;
}

ReturnStatus g__file_size(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
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
        return liberror(P, "__file_size", eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, "__file_size", eINVAL);
      case ENOENT:
        return liberror(P, "__file_size", eNOTFND);
      case EACCES:
        return liberror(P, "__file_size", eNOPERM);
      case ELOOP:
        return liberror(P, "__file_size", eINVAL);
      case EIO:
        return liberror(P, "__file_size", eIOERROR);
      case EFAULT:
        return liberror(P, "__file_size", eINVAL);
      default:
        return liberror(P, "__file_size", eNOTFND);
    }
  } else {
    termPo details = (termPo) allocateInteger(processHeap(P), buf.st_size);

    setProcessRunnable(P);

    ReturnStatus ret = {.ret=Ok, .result =details};

    return ret;
  }
}

ReturnStatus g__file_date(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
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
        return liberror(P, FILE_DATE, eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, FILE_DATE, eINVAL);
      case ENOENT:
        return liberror(P, FILE_DATE, eNOTFND);
      case EACCES:
        return liberror(P, FILE_DATE, eNOPERM);
      case ELOOP:
        return liberror(P, FILE_DATE, eINVAL);
      case EIO:
        return liberror(P, FILE_DATE, eIOERROR);
      case EFAULT:
        return liberror(P, FILE_DATE, eINVAL);
      default:
        return liberror(P, FILE_DATE, eNOTFND);
    }
  } else {
    heapPo H = processHeap(P);
    termPo atime = (termPo) allocateInteger(H, buf.st_atime);
    int root = gcAddRoot(H, &atime);
    termPo ctime = (termPo) allocateInteger(H, buf.st_ctime);
    gcAddRoot(H, &ctime);
    termPo mtime = (termPo) allocateInteger(H, buf.st_mtime);
    gcAddRoot(H, &mtime);
    normalPo triple = allocateTpl(H, 3);

    setArg(triple, 0, atime);
    setArg(triple, 1, ctime);
    setArg(triple, 2, mtime);
    gcReleaseRoot(H, root);

    setProcessRunnable(P);

    ReturnStatus ret = {.ret=Ok, .result =(termPo) triple};

    return ret;
  }
}

ReturnStatus g__file_modified(processPo P, ptrPo tos) {
  integer fnLen;
  const char *fn = stringVal(tos[0], &fnLen);
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
        return liberror(P, FILE_MODIFIED, eNOTDIR);
      case ENAMETOOLONG:
        return liberror(P, FILE_MODIFIED, eINVAL);
      case ENOENT:
        return liberror(P, FILE_MODIFIED, eNOTFND);
      case EACCES:
        return liberror(P, FILE_MODIFIED, eNOPERM);
      case ELOOP:
        return liberror(P, FILE_MODIFIED, eINVAL);
      case EIO:
        return liberror(P, FILE_MODIFIED, eIOERROR);
      case EFAULT:
        return liberror(P, FILE_MODIFIED, eINVAL);
      default:
        return liberror(P, FILE_MODIFIED, eNOTFND);
    }
  } else {
    termPo mtime = (termPo) allocateInteger(processHeap(P), buf.st_mtime);

    setProcessRunnable(P);

    ReturnStatus ret = {.ret=Ok, .result =mtime};

    return ret;
  }
}

ReturnStatus g__openInFile(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);

  if (file != Null) {
    ReturnStatus ret = {.ret=Ok, .result =(termPo) allocateIOChnnl(processHeap(P), file)};
    return ret;
  } else
    return liberror(P, "_openInFile", eNOTFND);
}

ReturnStatus g__openOutFile(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null) {
    ReturnStatus ret = {.ret=Ok, .result =(termPo) allocateIOChnnl(processHeap(P), file)};
    return ret;
  } else
    return liberror(P, "_openOutFile", eNOTFND);
}

ReturnStatus g__openAppendFile(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null) {
    ReturnStatus ret = {.ret=Ok, .result =(termPo) allocateIOChnnl(processHeap(P), file)};
    return ret;
  } else
    return liberror(P, "_openAppendFile", eNOTFND);
}

ReturnStatus g__openAppendIOFile(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  integer fnLen;
  const char *fn = stringVal(Arg1, &fnLen);
  char buff[MAXFILELEN];

  char *acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null) {
    ReturnStatus ret = {.ret=Ok, .result =(termPo) allocateIOChnnl(processHeap(P), file)};
    return ret;
  } else
    return liberror(P, "_openAppendIOFile", eNOTFND);
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

char *resolveFileName(char *cwd, const char *fn, integer fnLen, char *buff, integer buffLen) {
  if (fn[0] == '/') {
    uniNCpy(buff, buffLen, fn, fnLen);
    return buff;
  } else {
    char fname[MAXFILELEN];
    uniTrim(fn, fnLen, "", "/", fname, NumberOf(fname));
    fnLen = uniStrLen(fname);

    char wd[MAXFILELEN];
    uniTrim(cwd, uniStrLen(cwd), "", "/", wd, NumberOf(wd));
    integer wdLen = uniStrLen(wd);
    integer pos = 0;

    while (pos < fnLen && fname[pos] == '.') {
      if (pos < fnLen - 2 && fname[pos + 1] == '.' && fname[pos + 2] == '/') {
        integer last = uniLastIndexOf(wd, wdLen, '/');
        if (last >= 0) {
          wdLen = last;
          wd[last] = '\0';
          pos += 3;
        } else
          break;
      } else if (pos < fnLen - 1 && fname[pos + 1] == '/') {
        pos += 2;
      } else if (pos == fnLen - 1)
        pos++;
      else
        break;
    }
    strMsg(buff, buffLen, "%s/%s", wd, &fname[pos]);
    return buff;
  }
}
