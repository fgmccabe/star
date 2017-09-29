//
// Created by Francis McCabe on 6/16/17.
//

#include "engine.h"
#include "pkg.h"

packagePo loadedPackage(char *package);

static poolPo packagePool = NULL;
static hashPo loadedPackages = NULL;

void initPackages() {
  packagePool = newPool(sizeof(PackageRec), 128);
  loadedPackages = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

packagePo loadedPackage(char *package) {
  return (packagePo) hashGet(loadedPackages, package);
}

char *loadedVersion(char *package) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL)
    return (char *) &pkg->version;

  return NULL;
}

char *pkgName(packagePo pkg) {
  return (char *) &pkg->packageName;
}

char *pkgVers(packagePo pkg) {
  return (char *) &pkg->version;
}

static logical compatiblVersion(char *rqVer, char *ver) {
  return (logical) (uniCmp(rqVer, (char *) "*") == same || uniCmp(rqVer, ver) == same);
}

static packagePo markLoaded(char *package, char *version) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL) {
    if (!compatiblVersion((char *) &pkg->version, version))
      return Null;
    else
      return pkg;
  } else {
    pkg = (packagePo) allocPool(packagePool);
    uniCpy((char *) &pkg->packageName, NumberOf(pkg->packageName), package);
    uniCpy((char *) &pkg->version, NumberOf(pkg->version), version);
    hashPut(loadedPackages, &pkg->packageName, pkg);
    return pkg;
  }
}


// Implement escapes that access the manifest

/*
retCode g__pkg_is_present(processPo P, termPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);

  if (isvar(a1) || isvar(a2) || isvar(a3))
    return liberror(P, "_pkg_is_present", eSTRNEEDD);
  else if (!isString(objV(a1)) || !isString(objV(a2)) || !isString(objV(a3)))
    return liberror(P, "_pkg_is_present", eSTRNEEDD);
  else if (!isvar(a4))
    return liberror(P, "_pkg_is_present", eVARNEEDD);
  else {
    stringPo s1 = stringV(a1);
    stringPo s2 = stringV(a2);
    stringPo s3 = stringV(a3);

    byte pkgNm[MAX_SYMB_LEN];
    copyString2Buff(pkgNm, NumberOf(pkgNm), s1);

    byte verNm[MAX_SYMB_LEN];
    copyString2Buff(verNm, NumberOf(verNm), s2);

    byte kndNm[MAX_SYMB_LEN];
    copyString2Buff(kndNm, NumberOf(kndNm), s3);

    byte fn[MAXFILELEN];
    string actualFn = manifestResource(pkgNm, verNm, kndNm, fn, NumberOf(fn));

    if (actualFn == NULL)
      return Fail;
    else if (filePresent(actualFn) != Ok)
      return Fail;
    else {
      ptrI reslt = allocateCString(&P->proc.heap, (char *) actualFn);
      return equal(P, &reslt, &a[4]);
    }
  }
}

 */
