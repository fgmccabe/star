/*
  File management functions
  (c) 1994-2010 F.G. McCabe

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

  Contact: <fmccabe@gmail.com>
*/

#include "config.h"		/* Invoke configuration header */
#include "fileP.h"
#include "uri.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/dirent.h>

#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>		/* Where OPEN_MAX is defined */
#include <signal.h>

retCode rmFile(uniChar *name)
{
  long len = uniStrLen(name)*2+1;
  char fname[len];
  
  _utf(name,(unsigned char*)fname,len);
  
tryAgain:
  if(unlink(fname) != -1)
    return Ok;
  else
    switch(errno){
    case EINTR:
      goto tryAgain;
    default:
      return Error;
  }
}

retCode mvFile(uniChar *name,uniChar *to)
{
  long len = uniStrLen(name)*2+1;
  long tlen = uniStrLen(to)*2+1;
  char fname[len];
  char tname[tlen];
  
  _utf(name,(unsigned char*)fname,len);
  _utf(to,(unsigned char*)tname,tlen);
  
tryAgain:
  if(rename(fname,tname) != -1)
    return Ok;
  else
    switch(errno){
    case EINTR:
      goto tryAgain;
    default:
      return Error;
    }
}

retCode processDirectory(uniChar *url,dirProc proc,void *cl)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok && 
     uniIsLit(scheme,"file")){
    char fn[MAXLINE];
    struct stat buf;
    
    _utf(path,(unsigned char*)fn,NumberOf(fn));
  
    if(stat(fn, &buf) == -1)
      return Error;		/* File not found */
    else if(!S_ISDIR(buf.st_mode))
      return Fail;
    else{
      DIR *dir = opendir((const char*)fn);
      if(dir!=NULL){
	struct dirent *entry = readdir(dir);
	retCode ret = Ok;
	
	while(ret==Ok && entry!=NULL){
	  uniChar buffer[MAXLINE];
	  if(strncmp(entry->d_name,".",entry->d_namlen)!=0 &&
	     strncmp(entry->d_name,"..",entry->d_namlen)!=0){
	    int pos = utf8_uni((unsigned char*)entry->d_name,entry->d_namlen,
			       buffer,NumberOf(buffer));
	    buffer[pos] = '\0';

	    fileType type = entry->d_type==DT_REG? regularFileType
	      : entry->d_type==DT_DIR ? dirFileType
	      : entry->d_type==DT_LNK ? linkFileType
	      : entry->d_type==DT_SOCK ? socketFileType
	      : unknownFileType;
	    ret = proc(buffer,type,cl);
	  }	    
	  entry = readdir(dir);
	}
	closedir(dir);
	return ret;
      }
      else
	return Error;
    }
  }
  else
    return Error;
}

retCode regularFile(uniChar *name,char *fname,long len)
{
  struct stat buf;

  _utf(name,(unsigned char*)fname,len);
  
  if(stat(fname, &buf) == -1)
    return Fail;		/* File not found */
  else if(S_ISDIR(buf.st_mode))
    return Fail;
  else
    return Ok;
}

retCode isDirectoryFile(uniChar *name,char *fname,long len)
{
  struct stat buf;

  _utf(name,(unsigned char*)fname,len);
  
  if(stat(fname, &buf) == -1)
    return Error;		/* File not found */
  else if(S_ISDIR(buf.st_mode))
    return Ok;
  else
    return Fail;
}

retCode isDirectory(uniChar *name)
{
  char fn[MAXLINE];
  struct stat buf;
  _utf(name,(unsigned char*)fn,NumberOf(fn));

  if(stat(fn, &buf) == -1)
    return Error;		/* File not found */
  else if(S_ISDIR(buf.st_mode))
    return Ok;
  else
    return Fail;
}

fileType typeOfFile(uniChar *url)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok && 
     uniIsLit(scheme,"file")){
    char fn[MAXLINE];
    struct stat buf;
    _utf(path,(unsigned char*)fn,NumberOf(fn));

    if(stat(fn, &buf) == -1)
      return unknownFileType;		/* File not found */
    else if(S_ISDIR(buf.st_mode))
      return dirFileType;
    else if(S_ISREG(buf.st_mode))
      return regularFileType;
    else if(S_ISLNK(buf.st_mode))
      return linkFileType;
    else if(S_ISSOCK(buf.st_mode))
      return socketFileType;
  }
  return unknownFileType;
}

/* Special macro for Windows 95 */
#define FILE_ACCESS_MODE F_OK|R_OK

/* Check if a file is present or not */
logical filePresent(uniChar *name)
{
  char fn[MAXLINE];

  _utf(name,(unsigned char*)fn,NumberOf(fn));

  if(access(fn,FILE_ACCESS_MODE)==0)
    return True;
  else
    return False;
}
