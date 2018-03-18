//
// Created by Francis McCabe on 3/11/18.
//

#include <str.h>
#include <pkg.h>
#include <globals.h>
#include <stdlib.h>
#include <memory.h>
#include <stringBuffer.h>
#include <codeP.h>
#include <errorCodes.h>
#include <array.h>
#include <tpl.h>
#include "pkgops.h"

ReturnStatus g__pkg_is_present(processPo P, ptrPo tos) {
  char pkgNm[MAX_SYMB_LEN];
  char vers[MAX_SYMB_LEN];

  retCode ret = copyString2Buff(C_STR(tos[1]), pkgNm, NumberOf(pkgNm));
  if (ret == Ok)
    ret = copyString2Buff(C_STR(tos[0]), vers, NumberOf(vers));

  if (ret == Ok) {
    char *version = loadedVersion(pkgNm);

    if (version != NULL) {
      if (compatiblVersion(vers, version)) {
        ReturnStatus rt = {.ret=Ok, .rslt= trueEnum};
        return rt;
      }
    }

    ReturnStatus rt = {.ret=Ok, .rslt= falseEnum};
    return rt;
  } else {
    ReturnStatus rt = {.ret=ret, .rslt= voidEnum};
    return rt;
  }
}

typedef struct {
  heapPo H;
  listPo *list;
} pickupStruct;

static retCode pickupImport(char *pkgNm, char *vers, char *errorMsg, long msgLen, void *cl) {
  pickupStruct *pk = (pickupStruct *) cl;

  stringPo pkg = allocateCString(pk->H, pkgNm);
  int root = gcAddRoot(pk->H, (ptrPo) &pkg);

  stringPo vr = allocateCString(pk->H, vers);
  gcAddRoot(pk->H, (ptrPo) &vr);

  normalPo pr = allocatePair(pk->H, (termPo) pkg, (termPo) vr);
  *(pk->list) = addToList(pk->H, *pk->list, (termPo) pr);
  gcReleaseRoot(pk->H, root);
  return Ok;
}

ReturnStatus g__install_pkg(processPo P, ptrPo tos) {
  integer len;
  const char *text = stringVal(tos[0], &len);
  char *buffer = (char *) malloc(sizeof(char) * len);

  memmove(buffer, text, len);
  char errMsg[MAXLINE];
  heapPo H = processHeap(P);

  listPo imports = allocateList(H, 0, True);
  int root = gcAddRoot(H, (ptrPo) &imports);

  pickupStruct Cl = {.list=&imports, .H=H};

  retCode ret = installPackage(buffer, len, errMsg, NumberOf(errMsg), pickupImport, &Cl);

  free(buffer);

  if (ret == Ok) {
    ReturnStatus rt = {.ret=ret, .rslt= (termPo) imports};
    gcReleaseRoot(H, root);
    return rt;
  } else
    return liberror(P, "_install_pkg", eFAIL);
}
