//
// Created by Francis McCabe on 8/27/18.
//

#ifndef CAFE_STREAM_H
#define CAFE_STREAM_H

#include "io.h"

typedef struct _stream_object_ *streamPo;

extern classPo streamClass;

#ifdef VERIFY_OBJECT
#define O_STREAM(c) ((streamPo)(checkCast((c),streamClass)))
#else
#define O_STREAM(c) ((streamPo)(c))
#endif

#endif //CAFE_STREAM_H
