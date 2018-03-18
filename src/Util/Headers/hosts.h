/*
  Host name functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _LO_HOST_H_
#define _LO_HOST_H_

#include "object.h"
#include "unistr.h"

/* Host name management and interface */
char * getHostname(const char *name);
struct in_addr *getHostIP(char * name,int i);
char * getNthHostIP(char * name,unsigned long i,char * buffer,unsigned long len);
char * machineName(void);
char * machineIP(void);
logical isIPofHost(char * name,unsigned long ip);
void markHostUnavail(char * name);

// The public part of the host class interface

typedef struct _host_object_ *hostPo;
extern classPo hostClass;

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_HOST(c) ((hostPo)(checkCast((c),hostClass)))
#else
#define O_HOST(c) ((hostPo)(c))
#endif

#endif
