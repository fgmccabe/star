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

ValueReturn s__cwd(enginePo P)
{
  char cwBuffer[MAXFILELEN];
  strMsg(cwBuffer, NumberOf(cwBuffer), "%s/", processWd(P));
  return normalReturn(allocateString(processHeap(P), cwBuffer, uniStrLen(cwBuffer)));
}

ValueReturn s__cd(enginePo P, termPo dir)
{
  integer len;
  const char* cd = strVal(dir, &len);

  switch (setProcessWd(P, (char*)cd, len)){
  case Ok:
    return normalReturn(unitEnum);
  default:
    return abnormalReturn(eNOPERM);
  }
}

ValueReturn s__rm(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

tryAgain:
  switchProcessState(P, wait_io);

  if (unlink(acFn) != -1){
    setProcessRunnable(P);
    return normalReturn(unitEnum);
  }
  else{
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain;
    case EACCES:
    case EPERM:
      return abnormalReturn(eNOPERM);
    case EBUSY:
      return abnormalReturn(eFAIL);
    case ENOENT:
    default:
      return abnormalReturn(eIOERROR);
    }
  }
}

ValueReturn s__rmdir(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

tryAgain:
  switchProcessState(P, wait_io);

  if (rmdir(acFn) != -1){
    setProcessRunnable(P);
    return normalReturn(unitEnum);
  }
  else{
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain;
    case EACCES:
    case EPERM:
      return abnormalReturn(eNOPERM);
    case EBUSY:
      return abnormalReturn(eFAIL);
    case ENOENT:
    default:
      return abnormalReturn(eIOERROR);
    }
  }
}

ValueReturn s__mkdir(enginePo P, termPo f, termPo mde)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t mode = (mode_t)integerVal(mde);

tryAgain:
  switchProcessState(P, wait_io);

  if (mkdir(acFn, mode) != -1){
    setProcessRunnable(P);
    return normalReturn(unitEnum);
  }
  else{
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain;
    case EACCES:
    case EPERM:
      return abnormalReturn(eNOPERM);
    case EBUSY:
      return abnormalReturn(eFAIL);
    case ENOENT:
    default:
      return abnormalReturn(eIOERROR);
    }
  }
}

ValueReturn s__mv(enginePo P, termPo f, termPo t)
{
  integer sLen;
  const char* fn = strVal(f, &sLen);
  char srcBuff[MAXFILELEN];

  char* srcFn = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));
  integer dLen;
  const char* df = strVal(t, &dLen);
  char dstBuff[MAXFILELEN];

  char* dstFn = resolveFileName(processWd(P), df, dLen, dstBuff, NumberOf(dstBuff));

tryAgain:
  switchProcessState(P, wait_io);

  if (rename(srcFn, dstFn) != -1){
    setProcessRunnable(P);
    return normalReturn(unitEnum);
  }
  else{
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain;
    case EACCES:
    case EPERM:
      return abnormalReturn(eNOPERM);
    case EBUSY:
      return abnormalReturn(eFAIL);
    case ENOENT:
    default:
      return abnormalReturn(eIOERROR);
    }
  }
}

ValueReturn s__ls(enginePo P, termPo f)
{
  integer sLen;
  const char* fn = strVal(f, &sLen);
  char srcBuff[MAXFILELEN];

  char* dir = resolveFileName(processWd(P), fn, sLen, srcBuff, NumberOf(srcBuff));

  DIR* directory;

tryAgain:
  switchProcessState(P, wait_io);

  if ((directory = opendir(dir)) == NULL){
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain;
    case EACCES:
    case EMFILE:
    case ENFILE:
      return abnormalReturn(eNOPERM);
    case EBUSY:
      return abnormalReturn(eFAIL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case ENAMETOOLONG:
    case ENOTDIR:
      return abnormalReturn(eINVAL);
    default:
      return abnormalReturn(eIOERROR);
    }
  }
  else{
    termPo list = nilEnum;
    termPo name = voidEnum;
    heapPo h = processHeap(P);
    int root = gcAddRoot(h, &list);
    gcAddRoot(h, &name);

    struct dirent* ent;

    while ((ent = readdir(directory)) != NULL){
      /* skip special entries "." and ".." */
      if (strcmp(ent->d_name, ".") != 0 && strcmp(ent->d_name, "..") != 0){
        name = (termPo)allocateString(h, ent->d_name, uniStrLen(ent->d_name));
        list = (termPo)allocateCons(h, name, list);
      }
    }
    closedir(directory); /* Close the directory stream */

    gcReleaseRoot(h, root);
    setProcessRunnable(P);

    return normalReturn(list);
  }
}

ValueReturn s__file_mode(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1){
    setProcessRunnable(P);

    switch (errno){
    case EINTR:
      goto tryAgain;
    case ENOTDIR:
      return abnormalReturn(eNOFILE);
    case ENAMETOOLONG:
    case ELOOP:
    case EFAULT:
      return abnormalReturn(eINVAL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case EACCES:
      return abnormalReturn(eNOPERM);
    case EIO:
      return abnormalReturn(eIOERROR);
    default:
      return abnormalReturn(eNOTFND);
    }
  }
  else{
    setProcessRunnable(P);
    return normalReturn(makeInteger(buf.st_mode));
  }
}

ValueReturn s__file_chmod(enginePo P, termPo f, termPo m)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  mode_t acmode = (mode_t)integerVal(m);

tryAgain:
  switchProcessState(P, wait_io);

  if (chmod(acFn, acmode) == -1){
    setProcessRunnable(P);
    switch (errno){
    case EINTR:
      goto tryAgain; /* A mega hack */
    case EACCES:
    case EPERM:
    default:
      return abnormalReturn(eNOPERM);
    }
  }
  setProcessRunnable(P);
  return normalReturn(unitEnum);
}

ValueReturn s__file_present(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = filePresent(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);
  return normalReturn(present);
}

ValueReturn s__isdir(enginePo P, termPo d)
{
  integer fnLen;
  const char* fn = strVal(d, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  switchProcessState(P, wait_io);
  termPo present = isDirectory(acFn) == Ok ? trueEnum : falseEnum;
  setProcessRunnable(P);

  return normalReturn(present);
}

/*
 * file_type check out the type of the file
 */

typedef enum
{
  fifoFile = 0,
  directory = 1,
  charfile = 2,
  blockFile = 3,
  plainFile = 4,
  symLink = 5,
  fileSocket = 6
} FileType;

static char* const FILE_DATE = "__file_date";
static char* const FILE_MODIFIED = "__file_modified";

ValueReturn s__file_type(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1){
    setProcessRunnable(P);

    switch (errno){
    case EINTR:
      goto tryAgain;
    case ENOTDIR:
      return abnormalReturn(eNOFILE);
    case ENAMETOOLONG:
    case ELOOP:
    case EFAULT:
      return abnormalReturn(eINVAL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case EACCES:
      return abnormalReturn(eNOPERM);
    case EIO:
      return abnormalReturn(eIOERROR);
    default:
      return abnormalReturn(eNOTFND);
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
  else{
    return abnormalReturn(eINVAL);
  }
  return normalReturn(type);
}

ValueReturn s__file_size(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1){
    setProcessRunnable(P);

    switch (errno){
    case EINTR:
      goto tryAgain;
    case ENOTDIR:
      return abnormalReturn(eNOFILE);
    case ENAMETOOLONG:
    case ELOOP:
    case EFAULT:
      return abnormalReturn(eINVAL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case EACCES:
      return abnormalReturn(eNOPERM);
    case EIO:
      return abnormalReturn(eIOERROR);
    default:
      return abnormalReturn(eNOTFND);
    }
  }
  else{
    termPo details = makeInteger(buf.st_size);

    setProcessRunnable(P);
    return normalReturn(details);
  }
}

ValueReturn s__file_date(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1){
    setProcessRunnable(P);

    switch (errno){
    case EINTR:
      goto tryAgain;
    case ENOTDIR:
      return abnormalReturn(eNOFILE);
    case ENAMETOOLONG:
    case ELOOP:
    case EFAULT:
      return abnormalReturn(eINVAL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case EACCES:
      return abnormalReturn(eNOPERM);
    case EIO:
      return abnormalReturn(eIOERROR);
    default:
      return abnormalReturn(eNOTFND);
    }
  }
  else{
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
    return normalReturn((termPo)triple);
  }
}

ValueReturn s__file_modified(enginePo P, termPo f)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  struct stat buf;

tryAgain:
  switchProcessState(P, wait_io);

  if (stat(acFn, &buf) == -1){
    setProcessRunnable(P);

    switch (errno){
    case EINTR:
      goto tryAgain;
    case ENOTDIR:
      return abnormalReturn(eNOFILE);
    case ENAMETOOLONG:
    case ELOOP:
    case EFAULT:
      return abnormalReturn(eINVAL);
    case ENOENT:
      return abnormalReturn(eNOTFND);
    case EACCES:
      return abnormalReturn(eNOPERM);
    case EIO:
      return abnormalReturn(eIOERROR);
    default:
      return abnormalReturn(eNOTFND);
    }
  }
  else{
    termPo mtime = makeInteger(buf.st_mtime);

    setProcessRunnable(P);
    return normalReturn(mtime);
  }
}

ValueReturn s__openInFile(enginePo P, termPo f, termPo e)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);

  ioEncoding enc = pickEncoding(integerVal(e));
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInFile(acFn, enc);
  heapPo h = processHeap(P);

  if (file != Null){
    return normalReturn((termPo)allocateIOChnnl(h, file));
  }
  else{
    return abnormalReturn(eNOTFND);
  }
}

ValueReturn s__openOutFile(enginePo P, termPo f, termPo e)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);

  ioEncoding enc = pickEncoding(integerVal(e));
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openOutFile(acFn, enc);

  if (file != Null){
    return normalReturn((termPo)allocateIOChnnl(processHeap(P), file));
  }
  else{
    return abnormalReturn(eNOTFND);
  }
}

ValueReturn s__openAppendFile(enginePo P, termPo f, termPo e)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);

  ioEncoding enc = pickEncoding(integerVal(e));
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openAppendFile(acFn, enc);

  if (file != Null){
    return normalReturn((termPo)allocateIOChnnl(processHeap(P), file));
  }
  else{
    return abnormalReturn(eNOTFND);
  }
}

ValueReturn s__openAppendIOFile(enginePo P, termPo f, termPo e)
{
  integer fnLen;
  const char* fn = strVal(f, &fnLen);

  ioEncoding enc = pickEncoding(integerVal(e));
  char buff[MAXFILELEN];

  char* acFn = resolveFileName(processWd(P), fn, fnLen, buff, NumberOf(buff));

  ioPo file = openInOutAppendFile(acFn, enc);

  if (file != Null){
    return normalReturn((termPo)allocateIOChnnl(processHeap(P), file));
  }
  else{
    return abnormalReturn(eNOTFND);
  }
}

ioEncoding pickEncoding(integer k)
{
  switch (k){
  case 0:
    return rawEncoding;
  case 3:
    return utf8Encoding;
  default:
    return unknownEncoding;
  }
}
