#ifndef _ASM_H_
#define _ASM_H_

#include <ooio.h>

#include "config.h"
#include "assem.h"

extern logical debugAssem;
extern logical debugParse;

extern int getOptions(int argc, char **argv);

extern void setOutputFile(uniChar *path);

extern retCode parseContent(uniChar *path,uniChar *outPath);

extern void setCafeHome(uniChar *home);

#endif
