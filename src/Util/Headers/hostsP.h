/*
   Host machine tracking class -- the class keeps track of hosts. 
   Private header file
   (c) 1994-2006 Imperial College and F.G. McCabe

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
   Contact: Francis McCabe <fmccabe@gmail.com>
*/ 
#ifndef _HOST_P_LIB_H_
#define _HOST_P_LIB_H_

#include "retcode.h"
#include "logical.h"
#include "managedP.h"
#include "hosts.h"

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
} HostClassPartRec;

typedef struct _host_class_ {
  ObjectClassRec objectPart;		/* The base of the inheritance stack */
  ManagedClassPartRec managedPart;	/* We are a sub-class of managed */
  HostClassPartRec hostPart;		/* Our class variables */
} HostClassRec, *hostClassPo;

// Instance definitions

#define MAXALIAS 16
#define MAXHOST 16
#define MAXIP 10		/* Maximum number of IP addresses for one host */

typedef struct _host_part{
  uniChar host[MAXLINE];		/* Canonical name of this host */
  uniChar *aliases[MAXALIAS];	        /* list of aliases for this host */
  struct in_addr ip[MAXIP];	        /* ip address list for this host */
  unsigned int ip_count;	        /* How many addresses are there? */
  logical avail;		        /* True if this is a `positive' entry */
  long when;			        /* Used to retire a host address */
} HostRec;

// A Host object structure

typedef struct _host_object_ {
  ObjectRec object;			/* base is an object */
  ManagedRec managed;			/* we inherit from managed */
  HostRec host;				/* Our instance variables */
} HostObject;

extern HostClassRec HostClass;		/* Our class identifier */
  

#endif
  
