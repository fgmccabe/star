//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFESTP_H
#define LANDO_MANIFESTP_H

#include "manifest.h"

typedef struct _manifest_entry_ {
  char package[MAX_SYMB_LEN];
  hashPo versions;
} ManifestEntryRecord;

typedef struct _manifest_version_ {
  char version[MAXFILELEN];
  hashPo resources;
} ManifestVersionRecord;

typedef struct _manifest_file_name_ {
  char kind[MAX_SYMB_LEN];
  char fn[MAXFILELEN];
} ManifestFileRecord;


#endif //LANDO_MANIFESTP_H
