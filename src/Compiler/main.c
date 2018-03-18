/*
 * main program for the compiler
 */
#include "config.h"
#include <ooio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "compiler.h"
#include "compile.h"
#include "options.h"
#include "package.h"
#include "heap.h"
#include "lex.h"

char copyRight[]="(c) 2010-2013 F.G.McCabe\nAll rights reserved";

int main(int argc, char **argv)
{
  int narg;

#ifdef HAVE_LOCALECONV
  setlocale(LC_ALL,"");		/* set up locale */
#endif

#ifdef LOCALEDIR
  bindtextdomain(PACKAGE,LOCALEDIR);
  textdomain(PACKAGE);
#endif

  {
    char fn[]={'-',0};
    initLogfile(fn);
  }

  if((narg=getOptions(argc,argv))<0){
    cafeUsage(argv[0]);
    exit(1);
  }

  initCompiler();

  if(narg<argc){
    char buff[1024];
    _uni((unsigned char*)argv[narg],buff,NumberOf(buff));
    char pathBuff[MAXLINE];
    resolveURI(defaultURI(NULL),buff,pathBuff,NumberOf(pathBuff));
    char *path = uniIntern(pathBuff);

    initHeap(heapSize);

    retCode ret = compileAndGo(path,argc-narg-1,&argv[narg+1]);

    if(ret==Error)
      reportErrorCount();
  }
  else
    cafeUsage(argv[0]);
}


