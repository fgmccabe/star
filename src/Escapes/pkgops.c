//
// Created by Francis McCabe on 3/11/18.
//

#include <strings.h>
#include <pkgP.h>
#include <globals.h>
#include <stdlib.h>
#include <memory.h>
#include <stringBuffer.h>
#include <codeP.h>
#include <errorCodes.h>
#include <tpl.h>
#include <manifest.h>
#include <cons.h>
#include <consP.h>
#include "pkgops.h"

ReturnStatus g__pkg_is_present(heapPo h, termPo xc, termPo a1, termPo a2) {
  char pkgNm[MAX_SYMB_LEN];
  char vers[MAX_SYMB_LEN];

  retCode ret = copyChars2Buff(C_STR(a1), pkgNm, NumberOf(pkgNm));
  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a2), vers, NumberOf(vers));

  if (ret == Ok) {
    char *version = loadedVersion(pkgNm);

    if (version != NULL) {
      if (compatiblVersion(vers, version)) {
        return (ReturnStatus) {.ret=Normal, .result= trueEnum};
      }
    }

    return (ReturnStatus) {.ret=Normal, .result= falseEnum};
  } else {
    return (ReturnStatus) {.ret=Abnormal, .result= eNOTFND};
  }
}

typedef struct {
  heapPo H;
  termPo *list;
} pickupStruct;

static retCode pickupImport(packagePo p, char *errorMsg, long msgLen, void *cl) {
  pickupStruct *pk = (pickupStruct *) cl;

  if (!isLoadedPackage(p)) {
    termPo pkg = allocateCString(pk->H, p->packageName);
    int root = gcAddRoot(pk->H, &pkg);

    termPo vr = allocateCString(pk->H, p->version);
    gcAddRoot(pk->H, &vr);

    normalPo pr = allocatePair(pk->H, (termPo) pkg, vr);
    *(pk->list) = (termPo) allocateCons(pk->H, (termPo) pr, *(pk->list));
    gcReleaseRoot(pk->H, root);
  }
  return Ok;
}

ReturnStatus g__install_pkg(heapPo h, termPo xc, termPo a1) {
  integer len;
  const char *text = strVal(a1, &len);
  char *buffer = (char *) malloc(sizeof(char) * len);

  memmove(buffer, text, len);
  char errMsg[MAXLINE];

  termPo imports = (termPo) nilEnum;
  int root = gcAddRoot(h, &imports);

  pickupStruct Cl = {.list=&imports, .H=h};

  retCode ret = installPackage(buffer, len, h, errMsg, NumberOf(errMsg), pickupImport, &Cl);

  free(buffer);
  gcReleaseRoot(h, root);

  if (ret == Ok) {
    return (ReturnStatus) {.ret=Normal, .result= (termPo) imports};
  } else
    return (ReturnStatus) {.ret=Abnormal, .result=eFAIL};
}

ReturnStatus g__in_manifest(heapPo h, termPo xc, termPo a1, termPo a2, termPo a3) {
  char pkg[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
  char kind[MAX_SYMB_LEN];
  char rsrc[MAXFILELEN];

  retCode ret = copyChars2Buff(C_STR(a1), ((char *) (pkg)), NumberOf(pkg));
  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a2), ((char *) (version)), NumberOf(version));

  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a3), kind, NumberOf(kind));

  if (ret == Ok) {
    ret = manifestCompatibleResource(pkg, version, kind, rsrc, NumberOf(rsrc));

    if (ret != Ok) {
      return (ReturnStatus) {.ret=Normal, .result= falseEnum};
    } else {
      return (ReturnStatus) {.ret=Normal, .result= trueEnum};
    }
  } else {
    return (ReturnStatus) {.ret=Abnormal, .result= eIOERROR};
  }
}

ReturnStatus g__locate_in_manifest(heapPo h, termPo xc, termPo a1, termPo a2, termPo a3) {
  char pkg[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
  char kind[MAX_SYMB_LEN];
  char rsrc[MAXFILELEN];

  retCode ret = copyChars2Buff(C_STR(a1), pkg, NumberOf(pkg));
  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a2), version, NumberOf(version));

  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a3), kind, NumberOf(kind));

  if (ret == Ok) {
    ret = manifestCompatibleResource(pkg, version, kind, rsrc, NumberOf(rsrc));

    if (ret != Ok) {
      return (ReturnStatus) {.ret=Abnormal, .result= eNOTFND};
    } else {
      return (ReturnStatus) {.ret=Normal,
        .result=(termPo) allocateString(h, rsrc, uniStrLen(rsrc))};
    }
  } else {
    return (ReturnStatus) {.ret=Abnormal, .result= ioErrorCode(ret)};
  }
}

ReturnStatus g__globalIsSet(heapPo h, termPo a1) {
  integer llen;
  const char *lhs = strVal(a1, &llen);

  integer len = llen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  buff[llen] = '\0';

  globalPo global = globalVar(buff);

  return (ReturnStatus) {.ret=Normal, .result=(termPo) (global != Null && glbIsSet(global) ? trueEnum : falseEnum)};
}
