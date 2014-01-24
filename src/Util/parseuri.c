/*
  URI parsing functions
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
#include "uri.h"
#include "unicode.h"
#include <stdlib.h>

static int defaultPort(uniChar *scheme);

static void clear(uniChar *text,long len);

retCode parseURI(uniChar *uri, uniChar *scheme,long sLen,
		 uniChar *user,long uLen,uniChar *pass,long pLen,
		 uniChar *host,long hLen,long *port,
		 uniChar *path,long tLen,
		 uniChar *query,long qLen,uniChar *fragment,long fLen)
{
  long uriLen = uniStrLen(uri);
  long colonPos = uniIndexOf(uri,uriLen,':');

  if(colonPos>0){
    uniSubStr(uri,uriLen,0,colonPos,scheme,sLen); /* Extract the scheme */
    uri = &uri[colonPos+1];
    uriLen-=colonPos+1;			/* bump the start of the uri */

    if(uniIsLitPrefix(uri,"//")){
      uri = &uri[2];
      uriLen -= 2;			/* move over the // */
      // We have an authority ...
      long atPos = uniIndexOf(uri,uriLen,'@');
      if(atPos>0){
	uniSubStr(uri,uriLen,2,atPos,user,uLen);
	uri = &uri[atPos+1];
	uriLen -= atPos+1;
      }
      else
	clear(user,uLen);		/* No user component */
      long portPos = uniIndexOf(uri,uriLen,':');
      if(portPos>=0){
	uniSubStr(uri,uriLen,0,portPos++,host,hLen);
	long p = 0;
	while(portPos<uriLen && isNdChar(uri[portPos]))
	  p = p*10+digitValue(uri[portPos++]);
	*port = p;
	uri = &uri[portPos];
	uriLen -= portPos;
      }
      else{
	long slPos = uniIndexOf(uri,uriLen,'/');
	if(slPos>0){			/* We found a slash ... */
	  uniSubStr(uri,uriLen,0,slPos,host,hLen);
	  uri = &uri[slPos];
	  uriLen-=slPos;
	}
	else{				/* No path or anything */
	  uniNCpy(host,hLen,uri,uriLen);
	  uri = &uri[uriLen];
	  uriLen = 0;
	}
	*port = defaultPort(scheme);
      }
    }
    else{				/* No authority */
      clear(user,uLen);
      clear(host,hLen);
      *port = defaultPort(scheme);
    }
  }
  else{					/* unknown or implicit scheme */
    clear(scheme,sLen);
    clear(user,uLen);
    clear(host,hLen);
    *port = -1;
  }

  long queryPos = uniIndexOf(uri,uriLen,'?'); /* Is there a query? */
  if(queryPos>0){
    uniSubStr(uri,uriLen,0,queryPos,path,pLen);
    uri = &uri[queryPos+1];
    uriLen -= queryPos+1;

    long fragPos = uniIndexOf(uri,uriLen,'#'); /* Is there a fragment? */
    if(fragPos>0){
      uniSubStr(uri,uriLen,0,fragPos,query,qLen);
      uniSubStr(uri,uriLen,fragPos+1,uriLen-fragPos-1,fragment,fLen);
    }
    else{
      clear(fragment,fLen);
      uniNCpy(query,qLen,uri,uriLen);
    }
  }
  else{					/* No query, is there a fragment? */
    long fragPos = uniIndexOf(uri,uriLen,'#');
    clear(query,qLen);
    if(fragPos>0){
      uniSubStr(uri,uriLen,0,fragPos,path,pLen);
      uniSubStr(uri,uriLen,fragPos+1,uriLen-fragPos-1,fragment,fLen);
    }
    else{
      uniNCpy(path,pLen,uri,uriLen);
      clear(fragment,fLen);
    }
  }
  return Ok;
}

static void clear(uniChar *txt,long len)
{
  *txt = 0;
}

static int defaultPort(uniChar *scheme)
{
  if(uniIsLit(scheme,"http"))
    return 80;
  else if(uniIsLit(scheme,"ftp"))
    return 21;
  else if(uniIsLit(scheme,"nntp"))
    return 119;
  else if(uniIsLit(scheme,"telnet"))
    return 23;
  else if(uniIsLit(scheme,"gopher"))
    return 70;				// default port for GOPHER
  else if(uniIsLit(scheme,"wais"))
    return 210;
  else if(uniIsLit(scheme,"prospero"))
    return 1525;
  else
    return -1;
}
