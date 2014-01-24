/*
 * Resource accessing and handling.
 * Based on using URIs to specify individual resources
 */

#include "config.h"
#include <ooio.h>
#include "utils.h"
#include "resources.h"
#include <assert.h>

static hashPo transducers;
static uniChar *fileTransducer(uniChar *url);

uniChar *FILE, *HTTP;

void initTransducers()
{
  transducers = NewHash(7,(hashFun)uniHash,(compFun)uniCmp,NULL);
  FILE = mkInterned("file");
  HTTP = mkInterned("http");

  addTransducer(FILE,fileTransducer);
  //  addTransducer(HTTP,httpTransducer);
}

void addTransducer(uniChar *scheme,Transducer trans)
{
  Install(scheme,trans,transducers);
}

uniChar *accessResource(uniChar *url)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok){
    Transducer trans = Search(scheme,transducers);
    if(trans!=NULL)
      return trans(url);
    else
      return NULL;
  }
  else
    return NULL;
}

uniChar *fileTransducer(uniChar *url)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok){
    if(uniCmp(scheme,FILE)==0 && filePresent(path)){
      ioPo inFile = openInFile(path,unknownEncoding);
      if(inFile!=Null){
	stringPo buffer = openOutStr(unknownEncoding);
	uniChar ch;

	while(inChar(inFile,&ch)==Ok)
	  outChar(O_IO(buffer),ch);

	long len;
	string buff = getStrText(buffer,&len);
	string text = uniDup(buff,len);
	closeFile(O_IO(buffer));
	closeFile(inFile);
	return text;
      }
    }
  }
  return Null;
}
