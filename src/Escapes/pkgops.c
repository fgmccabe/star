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

ReturnStatus g__pkg_is_present(processPo P, heapPo h, termPo a1, termPo a2) {
  char pkgNm[MAX_SYMB_LEN];
  char vers[MAX_SYMB_LEN];

  retCode ret = copyChars2Buff(C_STR(a1), pkgNm, NumberOf(pkgNm));
  if (ret == Ok)
    ret = copyChars2Buff(C_STR(a2), vers, NumberOf(vers));

  if (ret == Ok) {
    char *version = loadedVersion(pkgNm);

    if (version != NULL) {
      if (compatiblVersion(vers, version)) {
        return (ReturnStatus) {.ret=Ok, .result= trueEnum};
      }
    }

    return (ReturnStatus) {.ret=Ok, .result= falseEnum};
  } else {
    return (ReturnStatus) {.ret=ret, .result= voidEnum};
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

ReturnStatus g__install_pkg(processPo P, heapPo h, termPo a1) {
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
    return (ReturnStatus) {.ret=ret, .result= (termPo) imports};
  } else
    return liberror(P, h, "_install_pkg", eFAIL);
}

ReturnStatus g__in_manifest(processPo P, heapPo h, termPo a1, termPo a2, termPo a3) {
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
      return (ReturnStatus) {.ret=Ok, .result= falseEnum};
    } else {
      return (ReturnStatus) {.ret=Ok, .result= trueEnum};
    }
  } else {
    return (ReturnStatus) {.ret=ret, .result= voidEnum};
  }
}

ReturnStatus g__locate_in_manifest(processPo P, heapPo h, termPo a1, termPo a2, termPo a3) {
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
      return (ReturnStatus) {.ret=Error, .result= voidEnum};
    } else {
      return (ReturnStatus) {.ret=Ok,
        .result=(termPo) allocateString(h, rsrc, uniStrLen(rsrc))};
    }
  } else {
    return (ReturnStatus) {.ret=ret, .result= voidEnum};
  }
}
