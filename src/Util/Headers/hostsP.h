/*
   Host machine tracking class -- the class keeps track of hosts. 
   Private header file
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 
#ifndef _HOST_P_LIB_H_
#define _HOST_P_LIB_H_

#include "retcode.h"
#include "logical.h"
#include "managedP.h"
#include "hosts.h"
#include "hash.h"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>		/* Basic network database stuff */
#include <sys/time.h>		/* used for retiring host names */
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/param.h>

// The private part of the host object interface

typedef struct {
  hostPo ourHost;			/* This is our machine ... */
  hashPo hostTable;
} HostClassPartRec;

typedef struct _host_class_ {
  ObjectClassRec objectPart;		/* The base of the inheritance stack */
  HostClassPartRec hostPart;		/* Our class variables */
} HostClassRec, *hostClassPo;

// Instance definitions

#define MAXALIAS 16
#define MAXHOST 16
#define MAXIP 10		/* Maximum number of IP addresses for one host */
#define MAXLINE 1024

typedef struct _host_part{
  char host[MAXLINE];		/* Canonical name of this host */
  char * aliases[MAXALIAS];	        /* list of aliases for this host */
  struct in_addr ip[MAXIP];	        /* ip address list for this host */
  unsigned int ip_count;	        /* How many addresses are there? */
  logical avail;		        /* True if this is a `positive' entry */
  long when;			        /* Used to retire a host address */
} HostRec;

// A Host object structure

typedef struct _host_object_ {
  ObjectRec object;			/* base is an object */
  HostRec host;				/* Our instance variables */
} HostObject;

extern HostClassRec HostClass;		/* Our class identifier */
  

#endif
  
