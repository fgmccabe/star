//
// Created by Francis McCabe on 3/11/18.
//

#include <str.h>
#include <pkgP.h>
#include <globals.h>
#include <stdlib.h>
#include <memory.h>
#include <stringBuffer.h>
#include <codeP.h>
#include <errorCodes.h>
#include <array.h>
#include <tpl.h>
#include <manifest.h>
#include "pkgops.h"

ReturnStatus g__pkg_is_present(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char pkgNm[MAX_SYMB_LEN];
  char vers[MAX_SYMB_LEN];

  retCode ret = copyString2Buff(C_STR(Arg1), pkgNm, NumberOf(pkgNm));
  if (ret == Ok)
    ret = copyString2Buff(C_STR(Arg2), vers, NumberOf(vers));

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
  listPo *list;
} pickupStruct;

static retCode pickupImport(packagePo p, char *errorMsg, long msgLen, void *cl) {
  pickupStruct *pk = (pickupStruct *) cl;

  if (!isLoadedPackage(p)) {
    stringPo pkg = allocateCString(pk->H, p->packageName);
    int root = gcAddRoot(pk->H, (ptrPo) &pkg);

    stringPo vr = allocateCString(pk->H, p->version);
    gcAddRoot(pk->H, (ptrPo) &vr);

    normalPo pr = allocatePair(pk->H, (termPo) pkg, (termPo) vr);
    *(pk->list) = appendToList(pk->H, *pk->list, (termPo) pr);
    gcReleaseRoot(pk->H, root);
  }
  return Ok;
}

ReturnStatus g__install_pkg(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer len;
  const char *text = stringVal(Arg1, &len);
  char *buffer = (char *) malloc(sizeof(char) * len);

  memmove(buffer, text, len);
  char errMsg[MAXLINE];
  heapPo H = processHeap(P);

  listPo imports = allocateList(H, 0);
  int root = gcAddRoot(H, (ptrPo) &imports);

  pickupStruct Cl = {.list=&imports, .H=H};

  retCode ret = installPackage(buffer, len, H, errMsg, NumberOf(errMsg), pickupImport, &Cl);

  free(buffer);
  gcReleaseRoot(H, root);

  if (ret == Ok) {
    return (ReturnStatus) {.ret=ret, .result= (termPo) imports};
  } else
    return liberror(P, "_install_pkg", eFAIL);
}

ReturnStatus g__in_manifest(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  char pkg[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
  char kind[MAX_SYMB_LEN];
  char rsrc[MAXFILELEN];

  retCode ret = copyString2Buff(C_STR(Arg1), ((char *) (pkg)), NumberOf(pkg));
  if (ret == Ok)
    ret = copyString2Buff(C_STR(Arg2), ((char *) (version)), NumberOf(version));

  if (ret == Ok)
    ret = copyString2Buff(C_STR(Arg3), kind, NumberOf(kind));

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

ReturnStatus g__locate_in_manifest(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  char pkg[MAX_SYMB_LEN];
  char version[MAX_SYMB_LEN];
  char kind[MAX_SYMB_LEN];
  char rsrc[MAXFILELEN];

  retCode ret = copyString2Buff(C_STR(Arg1), pkg, NumberOf(pkg));
  if (ret == Ok)
    ret = copyString2Buff(C_STR(Arg2), version, NumberOf(version));

  if (ret == Ok)
    ret = copyString2Buff(C_STR(Arg3), kind, NumberOf(kind));

  if (ret == Ok) {
    ret = manifestCompatibleResource(pkg, version, kind, rsrc, NumberOf(rsrc));

    if (ret!=Ok) {
      return (ReturnStatus) {.ret=Error, .result= voidEnum};
    } else {
      return (ReturnStatus) {.ret=Ok,
        .result=(termPo) allocateString(processHeap(P), rsrc, uniStrLen(rsrc))};
    }
  } else {
    return (ReturnStatus) {.ret=ret, .result= voidEnum};
  }
}
