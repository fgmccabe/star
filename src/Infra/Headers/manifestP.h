//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFESTP_H
#define LANDO_MANIFESTP_H

#include "manifest.h"
#include "starOptions.h"
#include "debug.h"

typedef struct _manifest_entry_ {
  char package[MAX_SYMB_LEN];
  hashPo versions;
} ManifestEntryRecord;

typedef struct _manifest_version_ {
  char version[MAXFILELEN];
  hashPo resources;
} ManifestVersionRecord;

typedef struct _manifest_resource_ {
  char kind[MAX_SYMB_LEN];
  char *fn;
  integer fnLen;
} ManifestRsrcRecord;

extern tracingLevel traceManifest;

retCode encodePkgName(ioPo out, packagePo pkg);

#endif //LANDO_MANIFESTP_H
