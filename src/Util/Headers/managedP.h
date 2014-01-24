/*
   Managed object class -- the class keeps track of all created instances. 
   Private header file
   (c) 1994-2004 Imperial College and F.G. McCabe

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
   
   Contact: Francis McCabe <frankmccabe@mac.com>
*/ 
#ifndef _MANAGED_P_LIB_H_
#define _MANAGED_P_LIB_H_

#include "retcode.h"
#include "logical.h"
#include "managed.h"
#include "objectP.h"

/* Class definition types */
typedef struct {
  managedPo instances;                  /* All the instances of the class */
} ManagedClassPartRec;

typedef struct _managed_class_ {
  ObjectClassRec objectPart;
  ManagedClassPartRec managedPart;
} ManagedClassRec;

/* Instance definition types */

typedef struct _managed_part{
  managedPo prev;                       /* previous in the chain */
  managedPo next;
} ManagedRec;

typedef struct _managed_object_{
  ObjectRec object;                     /* The object part of the managed object */
  ManagedRec managed;                   /* The managed part of the managed object */
} ManagedObject;

extern ManagedClassRec ManagedClass;    /* The managed class definition */
extern classPo managedClass;		/* The pointer to the managed class */

#endif
  
