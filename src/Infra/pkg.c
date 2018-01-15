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
