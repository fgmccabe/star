//
// Created by Francis McCabe on 2/1/17.
//

#ifndef STAR_MANIFEST_H
#define STAR_MANIFEST_H

#include "ooio.h"
#include "pkg.h"

typedef struct _manifest_entry_ *manifestEntryPo;
typedef struct _manifest_version_ *manifestVersionPo;
typedef struct _manifest_resource_ *manifestRsrcPo;

typedef retCode (*manifestProc)(char *name, void *cl);

manifestEntryPo manifestEntry(char *package);

retCode procVersions(manifestEntryPo entry,manifestProc,void *cl);

retCode manifestCompatibleResource(char *pkg, char *version, char *kind, char *buffer, integer buffLen);

retCode manifestResource(packagePo pkg, char *kind, char *buffer, integer buffLen);

retCode addToManifest(packagePo package, char *kind, char *resrc, integer length);
char *manifestOutPath(packagePo pkg, char *suff, char *buffer, int bufLen);
char *repoRsrcPath(char *name,char *buffer,int bufLen);

retCode dispManifest(ioPo out);
retCode flushManifest();

void defltRepoDir();
retCode loadManifest();

retCode pruneResources(char *kind);

retCode setManifestPath(char *path);
#endif //STAR_MANIFEST_H
