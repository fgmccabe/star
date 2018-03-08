//
// Created by Francis McCabe on 3/7/18.
//

#include <str.h>
#include "fileops.h"

ReturnStatus g__cwd(processPo p, ptrPo tos) {
  char *wd = processWd(p);
  termPo cwd = allocateString(processHeap(p), wd, uniStrLen(wd));

  ReturnStatus rtn = {.rslt = cwd, .ret=Ok};
  return rtn;
}

ReturnStatus g__cd(processPo p, ptrPo tos) {
  integer len;
  const char *cd = stringVal(tos[0], &len);

  return rtnStatus(p, setProcessWd(p, (char *) cd, len), "cd problem");
}

