//
// Created by Francis McCabe on 3/15/18.
//

#ifndef CAFE_COMPILEOPTIONS_H
#define CAFE_COMPILEOPTIONS_H

#include "ooio.h"

#ifdef ALLTRACE
#define TRACECODEGEN
#endif

#ifdef TRACECODEGEN
extern logical debugCodeGen;
#endif

extern logical compileOnly;
extern logical parseOnly;
extern logical traceManifest;

#endif //CAFE_COMPILEOPTIONS_H
