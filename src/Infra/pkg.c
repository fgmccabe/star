//
// Created by Francis McCabe on 6/16/17.
//

#include "pkgP.h"

char *pkgName(packagePo pkg) {
  return (char *) &pkg->packageName;
}

char *pkgVers(packagePo pkg) {
  return (char *) &pkg->version;
}

logical compatiblVersion(char *rqVer, char *ver) {
  return (logical)(uniCmp(rqVer, (char *) "*") == same || uniCmp(rqVer, ver) == same);
}

integer pkgHash(packagePo pkg){
  return hash64(uniHash(pkg->packageName)*37+uniHash(pkg->version));
}

comparison compPkg(packagePo p1,packagePo p2){
  comparison cmp = uniCmp(p1->packageName,p2->packageName);

  if(cmp==same)
    cmp = uniCmp(p1->version,p2->version);
  return cmp;
}

logical compatiblePkg(packagePo p1,packagePo p2){
  if(uniCmp(p1->packageName,p2->packageName)==same)
    return compatiblVersion(p1->version,p2->version);
  else
    return False;
}

retCode dispPkgNm(ioPo f, void *data, long depth, long precision, logical alt){
  packagePo pkg = (packagePo)data;

  if(uniIsLit(pkg->version,"*"))
    return outMsg(f,"%s",pkg->packageName);
  else
    return outMsg(f,"%s:%s",pkg->packageName,pkg->version);
}
