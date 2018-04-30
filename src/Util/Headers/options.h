//
// Created by Francis McCabe on 6/29/17.
//

#ifndef CAFE_OPTIONS_H
#define CAFE_OPTIONS_H

#include "retcode.h"
#include "logical.h"

void splitFirstArg(int argc, char **argv, int *newArgc, char ***newArgv);

typedef retCode (*setOption)(char *option,logical enable,void *cl);

typedef struct {
  char shortName;
  char *longName;
  logical hasArg;
  setOption setter;
  void *cl;
  char *usage;
} Option;

int processOptions(char *copyRight, int argc, char **argv, Option *options, int optionCount);

void showUsage(char *name, char *copyRight, Option options[], int optionCount);

#endif //CAFE_OPTIONS_H
