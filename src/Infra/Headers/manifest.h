//
// Created by Francis McCabe on 2/1/17.
//

#ifndef CAFE_MANIFEST_H
#define CAFE_MANIFEST_H

#include "ooio.h"
#include "pkg.h"

typedef struct _manifest_entry_ *manifestEntryPo;
typedef struct _manifest_version_ *manifestVersionPo;
typedef struct _manifest_resource_ *manifestRsrcPo;

manifestEntryPo manifestEntry(char *package);

char *manifestResource(char *package, char *version, char *kind);

char *manifestRsrcFlNm(char *package, char *version, char *kind, char *buffer, integer buffLen);

retCode addToManifest(packagePo package, char *kind, char *resrc);
char *manifestOutPath(packagePo pkg, char *suff, char *buffer, int bufLen);
char *repoRsrcPath(char *name,char *buffer,int bufLen);

retCode dumpManifest(ioPo out);
retCode flushManifest();

void defltRepoDir();
retCode loadManifest();

void setManifestPath(char *path);
#endif //CAFE_MANIFEST_H
