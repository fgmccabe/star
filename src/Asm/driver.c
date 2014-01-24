/*
 * main driver to parse and assemble a file
 */
#include "config.h"
#include "asm.h"
#include "errors.h"
#include "assemP.h"
#include "utils.h"
#include "parser.h"

#include "ooio.h"
#include <stdlib.h>

extern int ssparse (ioPo file,pkgPo context);

static void initStdUri();

retCode parseContent(uniChar *path,uniChar *outPath)
{
  initStdUri();
  ioPo file = openInFile(path, utf8Encoding);

  if(file!=Null){
    uniChar ch = inCh(file);		/* We skip over #! */

    if(ch=='#'){			/* look for standard #!/.... header */
      if((ch=inCh(file))=='!'){
	while((ch=inCh(file))!=uniEOF && ch!='\n')
	  ;			        /* consume the interpreter statement */
      }
      else{
	unGetChar(file,ch);
	unGetChar(file,'#');
      }
    }
    else
      unGetChar(file,ch);
    
    pkgPo pkg = newPkg(path);
    ssparse(file,pkg);

    if(debugAssem)
      dumpPkgCode(pkg);

    closeFile(file);			/* close the source string file */
    if(isErrorFree()){
      ioPo out = openOutFile(outPath,rawEncoding);
      retCode ret = encodePkg(out,pkg);

      closeFile(out);
      
      return ret;
    }
    else{
      outMsg(logFile,"output not written\n");
      return Fail;
    }
  }
  else
    return Fail;
}

static uniChar *CAFE_HOME = NULL;

void setCafeHome(uniChar *home)
{
  CAFE_HOME = home;
}

static ioPo openStdURI(uniChar *uri,ioEncoding encoding)
{
  uniChar scheme[MAXFILELEN];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(CAFE_HOME==NULL){
    outMsg(logFile,"CAFE_HOME not set");
    exit(99);
  }
  else if(parseURI(uri,scheme,NumberOf(scheme),
		   user,NumberOf(user),pass,NumberOf(pass),
		   host,NumberOf(host),&port,path,NumberOf(path),
		   query,NumberOf(query),frag,NumberOf(frag))==Ok &&
	  uniIsLit(scheme,"cafe")){
    uniChar newUri[MAXFILELEN];
    resolveURI(CAFE_HOME,path,newUri,NumberOf(newUri));
    return openURI(newUri,encoding);
  }
  else
    return NULL;
}

static void initStdUri()
{
  if(CAFE_HOME==NULL){
    char *home = getenv("CAFE_HOME");

    if(home!=NULL)
      CAFE_HOME = mkInterned(home);
    else{
      uniChar buff[MAXFILELEN];
      strMsg(buff,NumberOf(buff),"file:%s/share/stdlib/",CAFEDIR);
      CAFE_HOME = uniIntern(buff);
    }
  }
  registerTransducer(mkInterned("cafe"),openStdURI);
}

