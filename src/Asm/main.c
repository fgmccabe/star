/*
 * main program for the assembler
 */
#include <ooio.h>
#include "config.h"
#include "asm.h"
#include "assem.h"
#include "parser.h"
#include "asmOptions.h"
#include "errors.h"

#include <unicode.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

char copyRight[]="(c) 2010-2014 F.G.McCabe\nAll rights reserved";

static uniChar *outPath = Null;

static uniChar *computeOutputPath(uniChar *path);

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

  initAssem();

  if(narg<argc){
    uniChar path[1024];
    _uni((unsigned char*)argv[narg],path,NumberOf(path));

    parseContent(path,computeOutputPath(path));

    reportErrorCount();
  }
  else
    usage(argv[0]);
}

void setOutputFile(uniChar *path)
{
  outPath = uniIntern(path);
}

uniChar *computeOutputPath(uniChar *path)
{
  if(outPath==Null){
    long pathLen = uniStrLen(path);
    long lastPos = uniLastIndexOf(path,pathLen,'.');
    uniChar buff[4096];
    uniSubStr(path,pathLen,0,lastPos,buff,NumberOf(buff));
    uniTack(buff,NumberOf(buff),".co");
    return uniIntern(buff);
  }
  else
    return outPath;
}
