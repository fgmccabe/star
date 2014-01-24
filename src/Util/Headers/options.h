/***********************************************************************
(c) 2011 F.G. McCabe

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA.

Contact: frankmccabe@mac.com
***********************************************************************/

#ifndef _OOIO_OPTIONS_H_
#define _OOIO_OPTIONS_H_

#include "logical.h"
#include "retcode.h"
#include "logical.h"

typedef retCode (*optionFun)(char *option,logical enable,void *cl);

typedef struct _option_object_ {
  char shortName;			/* Single character name of option */
  char *longName;			/* Long name of option */
  logical hasArg;			/* Does this option have an argument */
  optionFun handler;			/* Handler for this option */
  void *cl;				/* Callback data for handler */
  char *usage;				/* String that describes this option */
} Option, *optionPo;

// Enable the convention -%-opt1%-opt2 for 
extern void splitFirstArg(int argc, char **argv,int *newArgc, char ***newArgv);

extern int processOptions(int argc, char **argv,Option options[],int numOpts);

extern int showUsage(char *name,Option options[],int numOpts);

#endif
