/*
 * main program for the run-time
 */
#include "config.h"
#include <ooio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "engine.h"

char copyRight[]="(c) 2010-2014 F.G.McCabe\nAll rights reserved";

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
    uniChar fn[]={'-',0};
    initLogfile(fn);
  }

  if((narg=getOptions(argc,argv))<0){
    usage(argv[0]);
    exit(1);
  }

  initHeap(heapSize);
  initEngine();

  if(narg<argc){
    uniChar *deflt = defaultURI(NULL);
    uniChar buff[MAXLINE],uriBuff[MAXLINE];
    _uni((unsigned char*)argv[narg],buff,NumberOf(buff));
    
    uniChar *boot = resolveURI(deflt,buff,uriBuff,NumberOf(uriBuff));

    return loadAndGo(boot,argc-narg-1,&argv[narg+1]);
  }
  else
    usage(argv[0]);
}
    
    
