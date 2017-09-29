/*
  Host name mgt and access
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <hostsP.h>
#include "io.h"

#define SECSINDAY 86400
#define RECHECK 60    /* We re-check unavailable hosts after 60 seconds */


// Implementation of the host class

static void initHostClass(classPo class, classPo request);
static void hostObject(objectPo o, va_list *args);
static void destroyHost(objectPo o);

HostClassRec HostClass = {
  {
    (classPo) &ObjectClass,    /* parent is managed */
    "host",        /* This is the host class */
    NULL,        /* Nothing special for inheritance */
    initHostClass,      /* This is what we need to init class */
    O_INHERIT_DEF,      /* host class object element creation */
    destroyHost,      /* host class object destruction */
    O_INHERIT_DEF,      /* nothing special for erasure */
    hostObject,        /* init of a host object */
    sizeof(HostObject),
    NULL,          /* pool of byte values */
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    PTHREAD_ONCE_INIT,        /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {          /* class variables for host */
    NULL,        /* our host record */
  }
};

classPo hostClass = (classPo) &HostClass;

/* Hash table of hosts */

static hostPo locateHost(char *name);

static void initHostClass(classPo class, classPo request) {
  ((hostClassPo) request)->hostPart.hostTable = NewHash(MAXHOST, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

static void retireHost(hostPo h) {
  destroyObject(O_OBJECT(h));
}

static void hostObject(objectPo o, va_list *args) {
}

static void destroyHost(objectPo o) {
  hostPo h = O_HOST(o);
  hostClassPo class = (hostClassPo) h->object.class;
  int i;

  lockClass((classPo) class);

  for (i = 0; i < MAXALIAS && h->host.aliases[i] != NULL; i++) {
    hashRemove(class->hostPart.hostTable, h->host.aliases[i]);
    free(h->host.aliases[i]);
  }

  hashRemove(class->hostPart.hostTable, h->host.host);

  unlockClass(h->object.class);
}

/* This will have to be upgraded to IPv6 */
static logical ipAddr(const char *addr, struct in_addr *ip) {
  int16 quads[4];
  char *p = (char *) addr;
  int i;

  for (i = 0; i < 4; i++) {
    quads[i] = (int16) strtol(p, &p, 10);  /* look for a quad number */

    if (*p == '.')
      p++;
    else if (*p != '\0' && i != 3)
      return False;
  }

  ip->s_addr = htonl(((quads[0] & 0xff) << 24) | ((quads[1] & 0xff) << 16) | ((quads[2] & 0xff) << 8) |
                     (quads[3] & 0xff));
  return True;
}

static hostPo locateHost(char *nme) {
  char nameBuff[MAXLINE], *name = nameBuff;

  uniLower(nme, uniStrLen(nme), nameBuff, NumberOf(nameBuff));
  hashPo htble = HostClass.hostPart.hostTable;

  hostPo h = (hostPo) hashGet(htble, name);  /* We try our own database first */

  struct hostent *he;
  struct timeval now;
  struct in_addr ip;

  gettimeofday(&now, NULL);

  if (h != NULL) {
    if (now.tv_sec > h->host.when)  /* should be be retiring this host name? */
      retireHost(h);    /* we have to re-compute this host */
    else {
      logical avail = h->host.avail;

      if (avail)
        return h;
      else {
#ifdef TRACEHOST
        logMsg(logFile,"%U marked as unavailable",nme);
#endif
        return NULL;    /* It was a negative entry ....!!!! */
      }
    }
  }

  if (ipAddr(name, &ip) &&  /* We have a name expressed in numbers and dots */
      (he = gethostbyaddr((char *) &ip, sizeof(struct in_addr), AF_INET)) != NULL) {
    unsigned int a = 0;
    char **al;

    h = (hostPo) newObject(hostClass);

    h->host.ip_count = 0;
    h->host.when = now.tv_sec + SECSINDAY;
    h->host.avail = True;
    strncpy((char *) h->host.host, he->h_name, NumberOf(h->host.host));

    lockClass(hostClass);       /* gethostbyname etc are not thread safe */
    hashPut(HostClass.hostPart.hostTable, h->host.host, h);

    he = gethostbyname(he->h_name); /* make sure that we get the proper name */

    for (al = he->h_aliases; *al != NULL && a < NumberOf(h->host.aliases); al++, a++) {
      h->host.aliases[a] = uniDuplicate((char *) al);
      hashPut(HostClass.hostPart.hostTable, h->host.aliases[a], h);
    }

    /* extract the IP numbers */
    for (al = he->h_addr_list; *al != NULL && h->host.ip_count < NumberOf(h->host.ip)
                               && a < NumberOf(h->host.aliases); al++) {
      h->host.ip[h->host.ip_count++] = *(struct in_addr *) (*al);
      h->host.aliases[a] = uniDuplicate(inet_ntoa(*(struct in_addr *) (*al)));
      hashPut(HostClass.hostPart.hostTable, h->host.aliases[a++], h); // put the IP in as an alias
    }

    h->host.aliases[a] = NULL;

    unlockClass(hostClass);

    return h;
  } else {
    /* We must now use the DNS to get the name */
    lockClass(hostClass);    /* a lot of non-thread safe stuff  */
    he = gethostbyname(name);  /* first stab ... */

    if (he != NULL && he->h_addr_list[0] != NULL) {
      unsigned int a = 0, i;
      char **al;

      h = (hostPo) newObject(hostClass);

      h->host.ip_count = 0;
      h->host.when = now.tv_sec + SECSINDAY;
      h->host.avail = True;

      strncpy((char *) h->host.host, he->h_name, NumberOf(h->host.host));
      hashPut(htble, h->host.host, h);

      for (al = he->h_aliases; *al != NULL && a < NumberOf(h->host.aliases); al++, a++) {
        h->host.aliases[a] = uniDuplicate(*al);
        hashPut(htble, h->host.aliases[a], h);
      }

      /* extract the IP numbers */
      for (al = he->h_addr_list; *al != NULL &&
                                 h->host.ip_count < NumberOf(h->host.ip) &&
                                 a < NumberOf(h->host.aliases); al++) {
        h->host.ip[h->host.ip_count++] = *(struct in_addr *) (*al);
        h->host.aliases[a] = uniDuplicate(inet_ntoa(*(struct in_addr *) (*al)));
        hashPut(htble, h->host.aliases[a++], h);
      }

      if (uniIndexOf(h->host.host, uniStrLen(h->host.host), 0, '.') < 0) { /* Use IP to get canonical name */
        for (i = 0; i < h->host.ip_count; i++) { /* We check each IP address ... */
          he = gethostbyaddr((char *) &h->host.ip[0], sizeof(struct in_addr), AF_INET);

          if (he != NULL) {
            unsigned int j;

            /* check out the new set of aliases */
            for (al = he->h_aliases; *al != NULL; al++) {
              logical found = False;

              for (j = 0; j < a && !found; j++) {
                if (uniIsLit(h->host.aliases[j], *al))
                  found = True;
              }

              if (!found && a < MAXALIAS) {
                h->host.aliases[a] = uniDuplicate(*al);
                hashPut(htble, h->host.aliases[a++], h);
              }
            }
          }
        }
      }

      h->host.aliases[a] = NULL;
      unlockClass(hostClass);
      return h;
    } else
      return NULL;
  }
}

void markHostUnavail(char *name) {
  hostPo h;

  if ((h = locateHost(name)) != NULL) {
    struct timeval now;

    gettimeofday(&now, NULL);

    h->host.avail = False;    /* host is off-line */
    h->host.when = now.tv_sec + RECHECK; /* when do we re-check the host */
  }
}

char *getHostname(char *name) {
  hostPo h;

  if ((h = locateHost(name)) == NULL)
    return NULL;
  else
    return h->host.host;
}

struct in_addr *getHostIP(char *name, int i) {
  hostPo h;

  if ((h = locateHost(name)) == NULL)
    return NULL;
  else if (i < h->host.ip_count)
    return &h->host.ip[i];
  else
    return NULL;
}

char *getNthHostIP(char *name, unsigned long i, char *buffer, unsigned long len) {
  hostPo h;

  if ((h = locateHost(name)) == NULL)
    return NULL;
  else if (i < h->host.ip_count) {
    strncpy((char *) buffer, inet_ntoa(h->host.ip[i]), len);

    return buffer;
  } else
    return NULL;
}

/* What is our name? */
char *machineName(void) {
  if (HostClass.hostPart.ourHost == NULL) {
    char hostNm[MAXHOSTNAMELEN + 1];

    if (gethostname((char *) hostNm, NumberOf(hostNm)) != 0)
      strcpy((char *) hostNm, "localhost");

    if ((HostClass.hostPart.ourHost = locateHost(hostNm)) == NULL)
      HostClass.hostPart.ourHost = locateHost((char *) "localhost");
  }

  if (HostClass.hostPart.ourHost != NULL)
    return HostClass.hostPart.ourHost->host.host;
  else
    return NULL;
}

/* What is our first IP number? */
char *machineIP(void) {
  if (HostClass.hostPart.ourHost == NULL) {
    return NULL;
  } else {
    return ((char *) inet_ntoa(HostClass.hostPart.ourHost->host.ip[0]));
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
