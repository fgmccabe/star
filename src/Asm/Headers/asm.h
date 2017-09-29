#ifndef _ASM_H_
#define _ASM_H_

#include <ooio.h>

#include "config.h"
#include "assem.h"

extern logical debugAssem;
extern logical debugParse;

extern int getOptions(int argc, char **argv);

extern void setOutputFile(char *path);

extern retCode parseContent(char *path,char *outPath);

extern void setCafeHome(char *home);

#endif
