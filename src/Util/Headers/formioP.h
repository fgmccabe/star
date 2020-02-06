/*
  Private header for the formatted I/O library
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_FORMIOP_H_
#define _IO_FORMIOP_H_

#include <stdarg.h>
#include "formio.h"

typedef retCode (*fileMsgProc)(ioPo f,void *data,long depth,long precision,logical alt);

void installMsgProc(codePoint key, fileMsgProc proc);
retCode __voutMsg(ioPo f, char *fmt, va_list args);

#endif

