/*
  URL management functions
  (c) 1999 F.G.McCabe

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

#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <pwd.h>

static inline void copyOut(uniChar *s,uniChar *d,uniChar *buff,long len)
{
  while(s<d && len-->0)
    *buff++=*s++;

  if(len>0)
    *buff = '\0';		/* terminate the output string */
}

uniChar *mergeURLs(uniChar *base,uniChar *url,uniChar *buffer,long len)
{
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXFILELEN],frag[MAXLINE];
  long port;
  urlScheme scheme;
  
  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))==Ok &&
     scheme!=unknownScheme){
    copyOut(url,uniEndStr(url),buffer,len);
    return buffer;
  }
  else{
    uniChar buser[MAXFILELEN];
    uniChar bpass[MAXFILELEN];
    uniChar bhost[MAXFILELEN];
    uniChar bpath[MAXLINE];
    uniChar bquery[MAXLINE];
    uniChar bfrag[MAXLINE];
    long bport;
    urlScheme bscheme;
    uniChar *pt;
    
    if(parseURI(base,&bscheme,buser,NumberOf(buser),bpass,NumberOf(bpass),
                bhost,NumberOf(bhost),&bport,bpath,NumberOf(bpath),
		bquery,NumberOf(bquery),bfrag,NumberOf(bfrag))!=Ok
       || bscheme==unknownScheme){
      if((pt=uniLast(base,uniStrLen(base),'/'))!=NULL && url[0]!='/'){
        uniNCpy(bpath,NumberOf(bpath),base,(pt-base));
        strMsg(buffer,len,"%U/%U",bpath,url);
        return buffer;
      }
      else{
        copyOut(url,uniEndStr(url),buffer,len);
        return buffer;
      }
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
    
    switch(bscheme){
      case httpScheme:{     // Construct an HTTP url
        if(bport!=80){
          if(uniStrLen(query)!=0)
            strMsg(buffer,len,"http://%U:%d/%U/%U",bhost,bport,bpath,url);
          else
            strMsg(buffer,len,"http://%U:%d/%U/%U",bhost,bport,bpath,url);
        }
        else{
          if(uniStrLen(query)!=0)
            strMsg(buffer,len,"http://%U/%U/%U",bhost,bpath,url);
          else
            strMsg(buffer,len,"http://%U/%U/%U",bhost,bpath,url);
        }
        return buffer;
      }
      case fileScheme:
        if(uniStrLen(bhost)!=0 && !uniIsLit(bhost,"localhost")){
          if(url[0]=='/')
            strMsg(buffer,len,"file://%U/%U",bhost,url);
          else
            strMsg(buffer,len,"file://%U/%U/%U",bhost,bpath,url);
        }
        else{
          if(url[0]=='/')
            strMsg(buffer,len,"file:///%U",url);
          else
            strMsg(buffer,len,"file:///%U/%U",bpath,url);
        }
        return buffer;
      case sysScheme:
        strMsg(buffer,len,"sys:%U/%U",bpath,url);
        return buffer;
      default:
        return NULL;
    }
  }
}

ioPo openURL(uniChar *sys,uniChar *url,ioEncoding encoding)
{
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;
  urlScheme scheme;

  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return openInFile(url,encoding);    /* try to load as a file */
  else{
    switch(scheme){
    case httpScheme:{                    // Locate an HTTP url
      return NULL;                    // Not implemented yet
    }
    case fileScheme:
    case unknownScheme:
      return openInFile(path,encoding); // For now we ignore the host
    case sysScheme:{
      uniChar fname[MAXFILELEN];
      
      strMsg(fname,NumberOf(fname),"%U/%U",sys,path);
      return openURL(sys,fname,encoding);
    }
    default:
      return NULL;                    /* not a recognized scheme */
    }
  }
}

ioPo findURL(uniChar *sys,uniChar *base,uniChar *url,ioEncoding encoding)
{
  uniChar ufn[2048];
  uniChar *actual = mergeURLs(base,url,ufn,NumberOf(ufn));
  return openURL(sys,actual,encoding);
}


ioPo createURL(uniChar *sys,uniChar *url,ioEncoding encoding)
{
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;
  urlScheme scheme;

  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return NULL;
  else{
    switch(scheme){
      case httpScheme:{                    // Locate an HTTP url
        return NULL;                    // Not implemented yet
      }
      case fileScheme:
    case unknownScheme:
        return openOutFile(url,encoding);       // For now we ignore the host
      case sysScheme:{
        uniChar fname[MAXFILELEN];

        strMsg(fname,NumberOf(fname),"%U/%U",sys,path);
        return createURL(sys,fname,encoding);
      }
      default:
        return NULL;
    }
  }
}

uniChar *defaultURL(uniChar *base)
{
  if(base!=NULL)
    return base;
  else{
    static uniChar CWD[MAXLINE]={0};

    if(uniStrLen(CWD)==0){
      char Buff[MAXLINE];
      char *pwd = getcwd(Buff,NumberOf(Buff));

      assert(pwd!=NULL);

      strMsg(CWD,NumberOf(CWD),"file:///%s/",pwd);
    }

    return CWD;
  }
}

retCode checkRoot(uniChar *sys,uniChar *root,uniChar *user)
{
  uniChar rootUser[MAXFILELEN],rootPass[MAXFILELEN];
  uniChar rootHost[MAXFILELEN],rootPath[MAXFILELEN];
  uniChar rootQuery[MAXLINE],rootFrag[MAXLINE];
  long rootPort;
  urlScheme rootScheme;
  
  uniChar userUser[MAXFILELEN],userPass[MAXFILELEN];
  uniChar userHost[MAXFILELEN],userPath[MAXFILELEN];
  uniChar userQuery[MAXLINE],userFrag[MAXLINE];
  long userPort;
  urlScheme userScheme;
    
  if(parseURI(root,&rootScheme,rootUser,NumberOf(rootUser),
	      rootPass,NumberOf(rootPass),
	      rootHost,NumberOf(rootHost),&rootPort,
	      rootPath,NumberOf(rootPath),
	      rootQuery,NumberOf(rootQuery),
	      rootFrag,NumberOf(rootFrag))!=Ok)
    return Error;
    
  if(parseURI(user,&userScheme,userUser,NumberOf(userUser),
	      userPass,NumberOf(userPass),
	      userHost,NumberOf(userHost),&userPort,
	      userPath,NumberOf(userPath),
	      userQuery,NumberOf(userQuery),
	      userFrag,NumberOf(userFrag))!=Ok)
    return Error;

  if(userScheme==sysScheme){	   
    uniChar fname[MAXFILELEN];
    
    strMsg(fname,NumberOf(fname),"%U/%U",sys,userPath);
    return checkRoot(sys,root,fname);
  }
  else if(userScheme==rootScheme){
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
	    return False;                /* we backed up too far */
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
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;
  urlScheme scheme;

  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return Error;
  else{
    switch(scheme){
    case httpScheme:{                    // Locate an HTTP url
      return Error;                   // Not implemented yet
    }
    case fileScheme:
    case unknownScheme:
      return filePresent(path);       // For now we ignore the host
    case sysScheme:{
      uniChar fname[MAXFILELEN];
      
      strMsg(fname,NumberOf(fname),"%U/%U",sys,path);
      return urlPresent(sys,fname);
    }
    default:
      return Error;
    }
  }
}

retCode rmURL(uniChar *sys,uniChar *url)
{
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  long port;
  urlScheme scheme;

  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),
	      query,NumberOf(query),frag,NumberOf(frag))!=Ok)
    return Error;
  else{
    switch(scheme){
      case httpScheme:{                    // Locate an HTTP url
        return Error;                   // Not implemented yet
      }
      case fileScheme:
        return rmFile(path);
      case sysScheme:{
        logMsg(logFile,"not permitted");
        return Error;
      }
      default:
        return Error;
    }
  }
}

retCode mvURL(uniChar *sys,uniChar *url,uniChar *nurl)
{
  uniChar user[MAXFILELEN],pass[MAXFILELEN];
  uniChar host[MAXFILELEN],path[MAXLINE];
  uniChar query[MAXLINE],frag[MAXLINE];
  uniChar nuser[MAXFILELEN],npass[MAXFILELEN];
  uniChar nhost[MAXFILELEN],npath[MAXLINE];
  uniChar nquery[MAXLINE],nfrag[MAXLINE];
  long port,nport;
  urlScheme scheme,nscheme;

  if(parseURI(url,&scheme,user,NumberOf(user),pass,NumberOf(pass),
	      host,NumberOf(host),&port,path,NumberOf(path),query,NumberOf(query),
	      frag,NumberOf(frag))!=Ok)
    return Error;
  else if(parseURI(nurl,&nscheme,nuser,NumberOf(nuser),npass,NumberOf(npass),
		   nhost,NumberOf(nhost),&nport,npath,NumberOf(npath),
		   nquery,NumberOf(nquery),nfrag,NumberOf(nfrag))!=Ok)
    return Error;
  else if(scheme!=nscheme)
    return Error;
  else{
    switch(scheme){
    case httpScheme:{                    // Locate an HTTP url
      return Error;                   // Not implemented yet
    }
    case fileScheme:
    case unknownScheme:
      return mvFile(path,npath);
    case sysScheme:{
      logMsg(logFile,"not permitted");
      return Error;
    }
    default:
      return Error;
    }
  }
}

uniChar *grabURL(uniChar *base,uniChar *url)
{
  ioPo in = openURL(base,url,utf8Encoding);
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
  
