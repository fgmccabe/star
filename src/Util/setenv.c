/*
  Our own setenv function
  (c) 1994-2000 Imperial College and F.G.McCabe

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

  Contact: Francis McCabe <fgm@fla.fujitsu.com>

  This function is compiled only if the system does not have a setenv
  function

 */
#include "config.h"
#ifndef HAVE_SETENV
#include <stdlib.h>
#include <string.h>
#include "logical.h"
#include "hash.h"


typedef struct _env_entry_{
  char *name;
  char *value;
} EnvRecord,*envPo;

static hashPo envs = NULL;

static void initEnv(void)
{
  static logical inited = False;

  if(!inited){
    inited = True;
    envs = NewHash(32,NULL,NULL,NULL);
  }
}    

int setenv(const char *name,const char *val,int overwrite)
{
  if(overwrite || getenv(name)==NULL){
    char *putBuffer = malloc(strlen(name)+strlen(val)+2);

    strcpy(putBuffer,name);
    strcat(putBuffer,"=");
    strcat(putBuffer,val);

    initEnv();

    {
      envPo e = hashGet(envs,(void*)name);
      
      if(e==NULL){
	e = (envPo)malloc(sizeof(EnvRecord));
	e->name = strdup(name);
	e->value = putBuffer;
	hashPut(envs,e->name,e);
      }
      else{
	free(e->value);
	e->value = putBuffer;
      }
    }

    return putenv(putBuffer);
  }
  else
    return 0;
}
#endif
