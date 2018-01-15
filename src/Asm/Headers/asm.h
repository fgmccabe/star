#ifndef _ASM_H_
#define _ASM_H_

#include <ooio.h>

#include "config.h"
#include "assem.h"

extern logical debugAssem;

extern int getOptions(int argc, char **argv);

extern retCode parseContent(char *path);

extern void setCafeHome(char *home);

extern char copyright[], version[];

#endif
