/*
 * The file catalog structure implementation
 */

#include "config.h"
#include "utils.h"
#include "catalogP.h"
#include <ooio.h>
#include <iostr.h>
#include <assert.h>

static string readFileContents(string base,string url);

static void *fileResolver(catalogPo cat,void *cl,uniChar *url);
static void fileAdder(catalogPo cat,void *cl,uniChar *name,void *content);
static retCode addEntry(uniChar *name,fileType type,void *cl);
static catalogEntryPo resolveFile(catalogPo catalog, uniChar *name);

catalogPo rootCatalog(uniChar *url)
{
  return fileSrcCatalog(url);
}

catalogPo fileSrcCatalog(uniChar *url)
{
  if(isDirectory(url)){
    catalogPo catalog = newCatalog(url,Null,fileResolver,Null,Null);
    processDirectory(base,addEntry,catalog);
    return catalog;
  }
  else
    return Null;
}

catalogPo fileBuildCatalog(uniChar *url)
{
  if(isDirectory(url)){
    catalogPo catalog = newCatalog(url,Null,fileResolver,fileAdder,Null);
    return catalog;
  }
  else
    return Null;
}

void *fileResolver(catalogPo cat,void *cl,uniChar *url)
{
  catalogEntryPo entry = resolveFile(cat,url);
  if(entry!=Null && entry->type==contentEntry)
    return entry->e.content;
  else
    return Null;
}

static catalogEntryPo resolveFile(catalogPo catalog, uniChar *name)
{
  catalogRefPo ref = Search(name,catalog->contents);
  if(ref!=Null){
    if(!ref->isLocal){
      uniChar urlBuff[MAXLINE];
      mergeURLs(catalog->url,ref->ref.url,urlBuff,NumberOf(urlBuff));
      switch(typeOfFile(urlBuff)){
      case regularFileType:{
	catalogEntryPo entry = addToCatalog(catalog,name,
					    readFileContents(catalog->url,
							     ref->ref.url));
	ref->isLocal = True;
	ref->ref.local = entry;
	return entry;
      }
      case dirFileType:{
	catalogPo subCat = fileSrcCatalog(urlBuff);
	if(subCat!=Null){
	  ref->isLocal = True;
	  catalogEntryPo entry = addSubCatalog(catalog,urlBuff,subCat);
	  ref->ref.local = entry;
	  return entry;
	}
	else
	  return Null;
      }
      case linkFileType:
      case socketFileType:
      default:
	return Null;
      }
    }
    return ref->ref.local;
  }
  else
    return Null;
}

static retCode addEntry(uniChar *name,fileType type,void *cl)
{
  catalogPo cat = (catalogPo)cl;

  switch(type){
  case regularFileType:{
    if(uniIsTail(name,cafeExtension)){
      catalogRefPo ref = (catalogRefPo)allocPool(refPool);
      ref->isLocal = False;
      long nameLen = uniStrLen(name);
      uniChar buffer[nameLen+1];
      uniSplit(name,0,nameLen-uniStrLen(cafeExtension),buffer,nameLen);
      ref->ref.url = uniIntern(name);
      
      Install(uniIntern(buffer),ref,cat->contents);
    }
    else if(uniIsTail(name,assemExtension)){
      catalogRefPo ref = (catalogRefPo)allocPool(refPool);
      ref->isLocal = False;
      long nameLen = uniStrLen(name);
      uniChar buffer[nameLen+1];
      uniSplit(name,0,nameLen-uniStrLen(assemExtension),buffer,nameLen);
      ref->ref.url = uniDuplicate(name);
      
      Install(uniIntern(buffer),ref,cat->contents);
    }
    else{
      catalogRefPo ref = (catalogRefPo)allocPool(refPool);
      ref->isLocal = False;
      ref->ref.url = uniDuplicate(name);
      
      Install(ref->ref.url,ref,cat->contents);
    }
    return Ok;
  }
  case linkFileType:
  case dirFileType:{
    catalogRefPo ref = (catalogRefPo)allocPool(refPool);
    ref->isLocal = False;
    ref->ref.url = uniDuplicate(name);
      
    Install(ref->ref.url,ref,cat->contents);
    return Ok;
  }
  default:
    return Ok;
  }
}


string readFileContents(string base,string url)
{
  ioPo inFile = openURL(base,url,unknownEncoding);
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
  else
    return Null;
}


static retCode firstLine(ioPo io,string text)
{
  outStr(io,"\"");
  while(text!=Null && *text!='\0' && *text!='\n')
    outChar(io,*text++);
  outStr(io,"...\"");
  return outChar(io,'\n');
}

