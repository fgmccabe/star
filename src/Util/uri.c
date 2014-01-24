/*
  URI management functions
  (c) 1999-2011 F.G.McCabe

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

  Contact: Francis McCabe <fmccabe@gmail.com>
 */

#include "config.h"
#include "fileP.h"
#include "uri.h"
#include "iostr.h"
#include "hash.h"

#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <pwd.h>

static hashPo transducers = NULL;

static ioPo openFileURI(uniChar *uri,ioEncoding encoding);

void initUri()
{
  if(transducers==NULL){
    transducers = NewHash(11,(hashFun)uniHash,(compFun)uniCmp,NULL);

    registerTransducer(uniNewStr((unsigned char*)"file"),openFileURI);
  }
}

retCode registerTransducer(uniChar *scheme,transducer transducer)
{
  return hashPut(transducers,uniIntern(scheme),transducer);
}


static inline void copyOut(uniChar *s,uniChar *d,uniChar *buff,long len)
{
  while(s<d && len-->0)
    *buff++=*s++;

  if(len>0)
    *buff = '\0';		/* terminate the output string */
}

static logical nonEmpty(uniChar *s)
{
  return *s!=0;
}

uniChar *resolveURI(uniChar *base,uniChar *url,uniChar *buffer,long len)
{
  uniChar scheme[MAXFILELEN];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;
  
  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok &&
     nonEmpty(scheme)){
    copyOut(url,uniEndStr(url),buffer,len);
    return buffer;			/* The uri is already resolved */
  }
  else{
    uniChar bscheme[MAXLINE];
    uniChar buser[MAXFILELEN];
    uniChar bpass[MAXFILELEN];
    uniChar bhost[MAXFILELEN];
    uniChar bpath[MAXLINE];
    uniChar bquery[MAXLINE];
    uniChar bfrag[MAXLINE];
    long bport;
    uniChar *pt;
    
    if(parseURI(base,bscheme,NumberOf(bscheme),
		buser,NumberOf(buser),bpass,NumberOf(bpass),
                bhost,NumberOf(bhost),&bport,bpath,NumberOf(bpath),
		bquery,NumberOf(bquery),bfrag,NumberOf(bfrag))!=Ok
       || uniIsLit(bscheme,"unknown")){
        copyOut(url,uniEndStr(url),buffer,len);
        return buffer;
    }
                      
    if((pt=uniLast(bpath,NumberOf(bpath),'/'))!=NULL)
      pt[0] = '\0';		/* chop off trailing stuff of base path */
    else{
      pt = &bpath[uniStrLen(bpath)];
      while(pt>bpath && *pt!='/' && *pt!=':')
        pt--;
      pt[0]='\0';
    }

    pt = path;

    if(uniStrLen(bhost)!=0){
      if(bport!=-1)
	strMsg(buffer,len,"%U://%U:%d/%U/%U",bscheme,bhost,bport,bpath,path);
      else
	strMsg(buffer,len,"%U://%U/%U/%U",bscheme,bhost,bpath,path);
    }
    else
      strMsg(buffer,len,"%U:%U/%U",bscheme,bpath,path);

    if(uniStrLen(query)!=0)
      strAppend(buffer,len,"?%U",query);

    if(uniStrLen(frag)!=0)
      strAppend(buffer,len,"#%U",frag);

    return buffer;
  }
}

ioPo openURI(uniChar *url,ioEncoding encoding)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return NULL;
  else{
    transducer trans = hashGet(transducers,scheme);

    if(trans!=NULL)
      return trans(url,encoding);
    else
      return NULL;
  }
}

ioPo openFileURI(uniChar *uri,ioEncoding encoding)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;

  if(parseURI(uri,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok)
    return openInFile(path,encoding);
  else
    return NULL;
}


uniChar *defaultURI(uniChar *base)
{
  if(base!=NULL)
    return base;
  else{
    static uniChar CWD[MAXLINE]={0};

    if(uniStrLen(CWD)==0){
      char Buff[MAXLINE];
      char *pwd = getcwd(Buff,NumberOf(Buff));

      assert(pwd!=NULL);

      strMsg(CWD,NumberOf(CWD),"file:%s/",pwd);
    }

    return CWD;
  }
}

retCode checkRoot(uniChar *sys,uniChar *root,uniChar *user)
{
  uniChar rootScheme[MAXLINE];
  uniChar rootUser[MAXFILELEN],rootPass[MAXFILELEN];
  uniChar rootHost[MAXFILELEN],rootPath[MAXFILELEN];
  uniChar rootQuery[MAXLINE],rootFrag[MAXLINE];
  long rootPort;

  uniChar userScheme[MAXLINE];
  uniChar userUser[MAXFILELEN],userPass[MAXFILELEN];
  uniChar userHost[MAXFILELEN],userPath[MAXFILELEN];
  uniChar userQuery[MAXLINE],userFrag[MAXLINE];
  long userPort;
    
  if(parseURI(root,rootScheme,NumberOf(rootScheme),
	      rootUser,NumberOf(rootUser),
	      rootPass,NumberOf(rootPass),
	      rootHost,NumberOf(rootHost),&rootPort,
	      rootPath,NumberOf(rootPath),
	      rootQuery,NumberOf(rootQuery),
	      rootFrag,NumberOf(rootFrag))!=Ok)
    return Error;
    
  if(parseURI(user,userScheme,NumberOf(userScheme),
	      userUser,NumberOf(userUser),
	      userPass,NumberOf(userPass),
	      userHost,NumberOf(userHost),&userPort,
	      userPath,NumberOf(userPath),
	      userQuery,NumberOf(userQuery),
	      userFrag,NumberOf(userFrag))!=Ok)
    return Error;

  if(uniIsLit(userScheme,"sys")){	   
    uniChar fname[MAXFILELEN];
    
    strMsg(fname,NumberOf(fname),"%U/%U",sys,userPath);
    return checkRoot(sys,root,fname);
  }
  else if(uniCmp(userScheme,rootScheme)==0){
    uniChar *uPath = userPath;

    /* In the first phase, we try to expand out the user path */
    if(*uPath=='~'){		/* Look for file relative to home */
      uPath++;
      if(*uPath=='/' || *uPath=='\0') {
      	char *home = getenv("HOME");
        if(home != NULL){
          uniChar uHome[MAXFILELEN];
          _uni((unsigned char*)home,uHome,NumberOf(uHome));
          uniInsert(uPath,NumberOf(userPath)-(uPath-userPath),uHome);
        }
      }
      else{
	char *rest,usr[512];
	struct passwd *pwd;

	_utf(uPath,(unsigned char*)usr,NumberOf(usr));	/* Extract the user's name */
	if((rest=strchr(usr, '/')) != NULL){
	  *rest = '\0';

	  pwd = getpwnam(usr);
	  
	  if(pwd!=NULL){
            uniChar uHome[MAXFILELEN];
	    _uni((unsigned char*)pwd->pw_dir,uHome,NumberOf(uHome));
	    uniTack(uHome,NumberOf(uHome),"/");
	    uniInsert(uPath,NumberOf(userPath)-(uPath-userPath),uHome); /* Append rest of name onto home dir */
	  }
	  else
	    return Fail;                 /* no user! */
	}
        else
          return Fail;                  /* user but no file! */
      }
    }

    if(*uPath=='/'){			/* Absolute path name -- must match root */
      if(uniNCmp(uPath,rootPath,uniStrLen(rootPath))!=0)
        return Fail;                    /* Absolute, but doesnt match root */
      else
        return Ok;                      /* We are OK */
    }
    else{				/* Relative file name */
      {
      	uniChar *e = uniEndStr(rootPath);
      	if(e!=NULL&&e>rootPath&&e[-1]=='/')
      	  e[-1]=0;                      /* chop off trailing / from cwd */
      }
      while(True){			/* consume .. and . from file name */
        if(uniIsLitPrefix(uPath,"../")){    /* We must back up one directory */
	  uniChar *pt;
	  
	  if(uniStrLen(rootPath)==0)
	    return Fail;                /* we backed up too far */
	  pt = uniLast(rootPath,NumberOf(rootPath),'/');
	  if(pt==NULL)
	    rootPath[0]='\0';            /* The last entry in the CWD */
	  else
	    *pt='\0';                   /* nibble off a piece of the CWD */
	  uPath+=3; 			/* step over the ../ segment */
	  continue;                     /* there may be more than one ../ */
	}
	else if(uniIsLitPrefix(uPath,"./")){
	  uPath+=2;                     /* step over */
	  continue;
	}
	else
          break;
      }
      return Ok;
    }
  }
  else
    return Fail;
}

retCode urlPresent(uniChar *sys,uniChar *url)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return Error;
  else{
    if(uniIsLit(scheme,"http"))
      return Error;                   // Not implemented yet
    else if(uniIsLit(scheme,"file")||uniIsLit(scheme,"unknown"))
      return filePresent(path)?Ok:Fail;       // For now we ignore the host
    else if(uniIsLit(scheme,"sys")){
      uniChar fname[MAXFILELEN];
      
      strMsg(fname,NumberOf(fname),"%U/%U",sys,path);
      return urlPresent(sys,fname);
    }
    else
      return Error;
  }
}

retCode rmURL(uniChar *sys,uniChar *url)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return Error;
  else{
    if(uniIsLit(scheme,"http"))
      return Error;                   // Not implemented yet
    else if(uniIsLit(scheme,"file")||uniIsLit(scheme,"unknown"))
      return rmFile(path);
    else if(uniIsLit(scheme,"sys")){
      logMsg(logFile,"not permitted");
      return Error;
    }
    else
      return Error;
  }
}

retCode mvURL(uniChar *sys,uniChar *url,uniChar *nurl)
{
  uniChar scheme[MAXLINE];
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  uniChar nscheme[MAXLINE];
  uniChar nuser[MAXFILELEN],npass[MAXFILELEN];
  uniChar nhost[MAXFILELEN],npath[MAXLINE];
  uniChar nquery[MAXLINE],nfrag[MAXLINE];
  long port,nport;

  if(parseURI(url,scheme,NumberOf(scheme),
	      user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),query,NumberOf(query),
	      frag,NumberOf(frag))!=Ok)
    return Error;
  else if(parseURI(nurl,nscheme,NumberOf(nscheme),
		   nuser,NumberOf(nuser),npass,NumberOf(npass),
		   nhost,NumberOf(nhost),&nport,npath,NumberOf(npath),
		   nquery,NumberOf(nquery),nfrag,NumberOf(nfrag))!=Ok)
    return Error;
  else if(uniCmp(scheme,nscheme)!=0)
    return Error;
  else if(uniIsLit(scheme,"http"))
    return Error;                   // Not implemented
  else if(uniIsLit(scheme,"file")||uniIsLit(scheme,"unknown"))
    return mvFile(path,npath);
  else if(uniIsLit(scheme,"sys")){
    logMsg(logFile,"not permitted");
    return Error;
  }
  else
    return Error;
}

uniChar *grabURI(uniChar *url)
{
  ioPo in = openURI(url,utf8Encoding);
  ioPo str = O_IO(openOutStr(utf16Encoding));
  long len;
  uniChar *text;
  
  while(isFileAtEof(in)!=Eof)
    outChar(str,inCh(in));
  outChar(str,0);
  
  text = getStrText(O_STRING(str),&len);
  
  text = uniDuplicate(text);
  
  closeFile(in);
  closeFile(str);
  
  return text;
}
  
