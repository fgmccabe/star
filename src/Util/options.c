/*
 * Some standard functions for processing command line options
 */

#include "config.h"		/* Invoke configuration header */
#include "options.h"
#include "file.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>

void splitFirstArg(int argc, char **argv,int *newArgc, char ***newArgv)
{
  /* 
     Splits the first command-line argument into multiple
     arguments if it starts with "-%". The
     delimiter is the first character following the percent sign.
     For example: "-%%-pdir1%-pdir2" will be split into
     two arguments "-pdir1", "-pdir2".

     This helps to work around the limitation that #! scripts
     can pass a maximum of one argument to the interpreter
     under certain operating systems (eg. Linux).
  */

  *newArgc = argc;
  *newArgv = argv;

  if (argc<2)
    return;

  if(strncmp(argv[1],"-%",2)==0){
    char delimiter = argv[1][2];
    int extra = 0, arg = 1;
    char *p;

    /* Count number of & in passed arguments */
    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) 
	break;

      p = q + 1;
      extra++;
    } while (*p != '\0');


    /* We didn't find any delimiters */
    if (extra == 0)
      return;

    /* Make the extra arguments */
    *newArgc = argc + extra;
    *newArgv = (char **) malloc(*newArgc * sizeof(char *));
    (*newArgv)[0] = argv[0];

    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) {
	(*newArgv)[arg++] = p;
	break;
      }
      else {
	int len = q - p;
	char *data = (char *) malloc(len + 1);
	
	strncpy(data, p, len);
	data[len] = '\0';
	(*newArgv)[arg++] = data;
	p = q + 1;
      }
    } while (True);
  }
}

int processOptions(int argc, char **argv, Option options[], int numOpts)
{
  int ix;

  for(ix=1;ix<argc;ix++) {
    char *opt = argv[ix];

    if(opt[0]=='-'){
      if(opt[1]=='-'){
	char *longName = &opt[2];	/* look for a long form name */
	if(strcmp(longName,"")==0)	/* -- on its own is end of options */
	  return ix+1;
	else{
	  for(int ox=0;ox<numOpts;ox++){
	    if(options[ox].longName!=NULL &&
	       strcmp(options[ox].longName,longName)==0){
	      if(options[ox].hasArg){
		if(ix<argc-1){
		  if(options[ox].handler(argv[++ix],False,options[ox].cl)!=Ok)
		    return showUsage(argv[0],options,numOpts);
		}
		else
		  return showUsage(argv[0],options,numOpts);
	      }
	      else if(options[ox].handler(NULL,False,options[ox].cl)!=Ok)
		return showUsage(argv[0],options,numOpts);
	      goto argLoop;
	    }
	  }
	  return showUsage(argv[0],options,numOpts);
	}
      }
      else if(opt[1]!='\0'){
	char shortName = opt[1];
	char *optArg = &opt[2];

	for(int ox=0;ox<numOpts;ox++){
	  if(options[ox].shortName==shortName){
	    if(options[ox].hasArg){
	      if(strcmp(optArg,"")==0){
		if(ix<argc-1)
		  optArg = argv[++ix];
		else
		  return showUsage(argv[0],options,numOpts);
	      }

	      if(options[ox].handler(optArg,False,options[ox].cl)!=Ok)
		return showUsage(argv[0],options,numOpts);
	    }
	    else{
	      if(options[ox].handler(NULL,False,options[ox].cl)!=Ok)
		return showUsage(argv[0],options,numOpts);
	    }
	    goto argLoop;
	  }
	}
	return showUsage(argv[0],options,numOpts);
      }
      else
	return showUsage(argv[0],options,numOpts);
  argLoop:;
    }
    else
      return ix;
  }
  return ix;
}

int showUsage(char *name,Option options[],int numOpts)
{
  char *sep = "";
  outMsg(logFile,"\nusage: %s ",name);
  for(int ix=0;ix<numOpts;ix++){
    outStr(logFile,sep);
    outStr(logFile,options[ix].usage);
    sep = " ";
  }
  outStr(logFile,"\n");
  flushOut();
  return -1;
}
