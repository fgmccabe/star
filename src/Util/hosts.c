/*
  Host name mgt and access
  (c) 1994-2006 Imperial College and F.G. McCabe

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

  Contact: fmccabe@gmail.com
*/

#include "config.h"		/* Invoke configuration header */
#include "hash.h"
#include "io.h"

#include "hostsP.h"


#define SECSINDAY 86400
#define RECHECK 60		/* We re-check unavailable hosts after 60 seconds */


// Implementation of the host class

static void initHostClass(classPo class,classPo request);
static void hostObject(objectPo o,va_list *args);
static void destroyHost(objectPo o);

HostClassRec HostClass = {
  {
    (classPo)&ManagedClass,		/* parent is managed */
    "host",				/* This is the host class */
    NULL,				/* Nothing special for inheritance */
    initHostClass,			/* This is what we need to init class */
    O_INHERIT_DEF,			/* host class object element creation */
    destroyHost,			/* host class object destruction */
    O_INHERIT_DEF,			/* nothing special for erasure */
    hostObject,				/* init of a host object */
    sizeof(HostObject),
    NULL,				  /* pool of byte values */
    PTHREAD_ONCE_INIT,			  /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL				/* chain of managed objects */
  },
  {					/* class variables for host */
    NULL,				/* our host record */
  }
};

classPo hostClass = (classPo)&HostClass;

static hashPo htble;			/* Hash table of hosts */

static hostPo locateHost(uniChar *name);

static void initHostClass(classPo class,classPo request)
{
}

static void retireHost(hostPo h)
{
  destroyObject(O_OBJECT(h));
}

static void hostObject(objectPo o,va_list *args)
{
}

static void destroyHost(objectPo o)
{
  hostPo h = O_HOST(o);
  int i;
  
  lockClass(h->object.class);

  for(i=0;i<MAXALIAS && h->host.aliases[i]!=NULL;i++){
    Uninstall(h->host.aliases[i],htble);
    free(h->host.aliases[i]);
  }

  Uninstall(h->host.host,htble);

  unlockClass(h->object.class);
}

/* This will have to be upgraded to IPv6 */
static logical ipAddr(const uniChar *addr,struct in_addr* ip)
{
  short quads[4];
  unsigned char aBuff[MAXLINE];
  char *p = (char*)_utf(addr,aBuff,NumberOf(aBuff));
  int i;

  for(i=0;i<4;i++){
    quads[i]=strtol(p,&p,10);	/* look for a quad number */

    if(*p=='.')
      p++;
    else if(*p!='\0' && i!=3)
      return False;
  }

  ip->s_addr = htonl(((quads[0]&0xff)<<24)|((quads[1]&0xff)<<16) | ((quads[2]&0xff)<<8) |
    (quads[3]&0xff));
  return True;
}


static hostPo locateHost(uniChar *nme)
{
  hostPo h = NULL;
  uniChar nameBuff[MAXLINE],*name=uniLower(nme,nameBuff,NumberOf(nameBuff));

  if(htble==NULL){
    htble = NewHash(MAXHOST,(hashFun)uniHash,(compFun)uniCmp,NULL);
  }
  else
    h = (hostPo)Search(name,htble);	/* We try our own database first */

  struct hostent *he;
  struct timeval now;
  struct in_addr ip;

  gettimeofday(&now,NULL);

  if(h!=NULL){
    if(now.tv_sec>h->host.when)	/* should be be retiring this host name? */
      retireHost(h);		/* we have to re-compute this host */
    else{
      logical avail = h->host.avail;

      if(avail)
	return h;
      else{
#ifdef TRACEHOST  
	logMsg(logFile,"%U marked as unavailable",nme);
#endif
	return NULL;		/* It was a negative entry ....!!!! */
      }
    }
  }

  if(ipAddr(name,&ip) &&	/* We have a name expressed in numbers and dots */
     (he=gethostbyaddr((char*)&ip,sizeof(struct in_addr),AF_INET))!=NULL){
    unsigned int a = 0;
    char **al;

    h = (hostPo)newObject(hostClass);

    h->host.ip_count = 0;
    h->host.when = now.tv_sec+SECSINDAY;
    h->host.avail = True;

    _uni((unsigned char*)he->h_name,h->host.host,NumberOf(h->host.host));

    lockClass(hostClass);	     /* gethostbyname etc are not thread safe */
    Install(h->host.host,h,htble);

    he = gethostbyname((char*)he->h_name); /* make sure that we get the proper name */

    for(al=he->h_aliases;*al!=NULL && a<NumberOf(h->host.aliases);al++,a++){
      uniChar alias[MAXLINE];

      _uni((unsigned char*)*al,alias,NumberOf(alias));
      h->host.aliases[a]=uniDuplicate(alias);
      Install(h->host.aliases[a],h,htble);
    }

    /* extract the IP numbers */
    for(al=he->h_addr_list;*al!=NULL && h->host.ip_count<NumberOf(h->host.ip) 
	  && a<NumberOf(h->host.aliases);al++){
      uniChar IP[MAXLINE];
      
      h->host.ip[h->host.ip_count++]=*(struct in_addr*)(*al);
      h->host.aliases[a]=uniDuplicate(_uni((unsigned char*)inet_ntoa(*(struct in_addr*)(*al)),IP,NumberOf(IP)));
      Install(h->host.aliases[a++],h,htble); /* put the IP in as an alias */
    }

    h->host.aliases[a]=NULL;

    unlockClass(hostClass);

    return h;
  }
  else{
    /* We must now use the DNS to get the name */
    char nBuff[MAXLINE];
    
    _utf(name,(unsigned char *)nBuff,NumberOf(nBuff));

    lockClass(hostClass);		/* a lot of non-thread safe stuff  */
    he = gethostbyname(nBuff);	/* first stab ... */

    if(he!=NULL && he->h_addr_list[0]!=NULL){
      unsigned int a = 0,i;
      char **al;

      h = (hostPo)newObject(hostClass);

      h->host.ip_count = 0;
      h->host.when = now.tv_sec+SECSINDAY;
      h->host.avail = True;

      _uni((unsigned char*)he->h_name,h->host.host,NumberOf(h->host.host));
      
      Install(h->host.host,h,htble);

      for(al=he->h_aliases;*al!=NULL&&a<NumberOf(h->host.aliases);al++,a++){
	uniChar alias[MAXLINE];

	h->host.aliases[a]=uniDuplicate(_uni((unsigned char*)*al,alias,NumberOf(alias)));
	Install(h->host.aliases[a],h,htble);
      }

      /* extract the IP numbers */
      for(al=he->h_addr_list;*al!=NULL && 
	    h->host.ip_count<NumberOf(h->host.ip) && 
	    a<NumberOf(h->host.aliases);al++){
	uniChar ip[MAXLINE];
	h->host.ip[h->host.ip_count++]=*(struct in_addr*)(*al);
	h->host.aliases[a]=uniDuplicate(_uni((unsigned char*)inet_ntoa(*(struct in_addr*)(*al)),ip,NumberOf(ip)));
	Install(h->host.aliases[a++],h,htble); /* put the IP in as an alias */
      }

      if(uniSearch(h->host.host,uniStrLen(h->host.host),'.')==NULL){ /* Use IP to get canonical name */
	for(i=0;i<h->host.ip_count;i++){ /* We check each IP address ... */
	  he = gethostbyaddr((char*)&h->host.ip[0],sizeof(struct in_addr),AF_INET);

	  if(he!=NULL){
	    unsigned int j;

	    /* check out the new set of aliases */
	    for(al = he->h_aliases; *al!=NULL; al++){
	      logical found=False;	

	      for(j=0;j<a && !found;j++){
		if(uniIsLit(h->host.aliases[j],*al))
		  found=True;
	      }

	      if(!found && a<MAXALIAS){
		uniChar alias[MAXLINE];

		h->host.aliases[a]=uniDuplicate(_uni((unsigned char*)*al,alias,NumberOf(alias)));
		Install(h->host.aliases[a++],h,htble);
	      }
	    }
	  }
	}
      }
    
      h->host.aliases[a]=NULL;
      unlockClass(hostClass);
      return h;
    }
    else
      return NULL;
  }
}

void markHostUnavail(uniChar *name)
{
  hostPo h;

  if((h=locateHost(name))!=NULL){
    struct timeval now;

    gettimeofday(&now,NULL);

    h->host.avail = False;		/* host is off-line */
    h->host.when = now.tv_sec+RECHECK; /* when do we re-check the host */
  }
}
  

uniChar *getHostname(uniChar *name)
{
  hostPo h;

  if((h=locateHost(name))==NULL)
    return NULL;
  else
    return h->host.host;
}

struct in_addr *getHostIP(uniChar *name,int i)
{
  hostPo h;

  if((h=locateHost(name))==NULL)
    return NULL;
  else if(i<h->host.ip_count)
    return &h->host.ip[i];
  else
    return NULL;
}  

uniChar *getNthHostIP(uniChar *name,unsigned long i,uniChar *buffer,unsigned long len)
{
  hostPo h;

  if((h=locateHost(name))==NULL)
    return NULL;
  else if(i<h->host.ip_count){
    _uni((unsigned char*)inet_ntoa(h->host.ip[i]),buffer,len);

    return buffer;
  }
  else
    return NULL;
}

/* What is our name? */
uniChar *machineName(void)
{
  if(HostClass.hostPart.ourHost==NULL){
    uniChar mbuff[MAXLINE];
    char hostNm[MAXHOSTNAMELEN+1];
    
    if(gethostname(hostNm,NumberOf(hostNm))!=0)
      _uni((unsigned char*)"localhost",mbuff,NumberOf(mbuff));
    else
      _uni((unsigned char*)hostNm,mbuff,NumberOf(mbuff));

    if((HostClass.hostPart.ourHost = locateHost(mbuff))==NULL){
      uniChar lname[] = {'l','o','c','a','l','h','o','s','t',0};
      HostClass.hostPart.ourHost = locateHost(lname);
    }
  }

  if(HostClass.hostPart.ourHost!=NULL)
    return HostClass.hostPart.ourHost->host.host;
  else
    return NULL;
}

/* What is our first IP number? */
uniChar *machineIP(void)
{
  if(HostClass.hostPart.ourHost==NULL){
    return NULL;
  }
  else{
    static uniChar IP[MAXLINE];

    _uni((unsigned char*)inet_ntoa(HostClass.hostPart.ourHost->host.ip[0]),IP,NumberOf(IP));

    return IP;
  }
}

#ifdef TRACEHOST  
static void dH(void *n,void *r,void *c)
{
  char *name = (char*)n;
  hostPo host = (hostPo)r;
  filePo f = (filePo)c;
  int i;
  
  lock(O_OBJECT(host));

  outMsg(f,"%s:\n",name);
  for(i=0;host->aliases[i]!=NULL;i++)
    outMsg(f," -> %U\n",host->aliases[i]);

  for(i=0;i<host->ip_count;i++)
    outMsg(f," @ %s\n",inet_ntoa(host->ip[i]));

  unlock(O_OBJECT(host));
}

extern ioPo logFile;
void dumpHosts()
{
  processHash(dH,htble,logFile);
  flushFile(logFile);
}
#endif
