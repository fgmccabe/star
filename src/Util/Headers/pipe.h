/*
   Pipeclass interface
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
*/ 

#ifndef IO_PIPE_LIB_H_
#define IO_PIPE_LIB_H_

#include "config.h"
#include "file.h"

typedef struct pipe_object_ *pipePo;

retCode openPipe(char *exec,char **argv,char **envv,ioPo *inpipe,ioPo *outpipe,ioPo *errpipe,
                 ioEncoding encoding);

logical isAPipe(objectPo p);

#endif

