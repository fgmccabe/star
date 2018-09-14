/*
  UDP socket handling functions
  (c) 1994-2017 F.G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
  Contact: Francis McCabe <frankmccabe@mac.com>
*/
#include "config.h"		/* pick up standard configuration header */
#include "iosockP.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "udpP.h"
#include "hosts.h"

#ifdef SYSV
#include <stropts.h>
#endif

#include <errno.h>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR (-1)
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET (-1)
#endif

static void inheritUDP(classPo class, classPo request);
static void initUDPClass(classPo class, classPo request);
static void UdpInit(objectPo o, va_list *args);
static retCode udpReadyIn(udpPo f);
static retCode udpReadyOut(udpPo f);
static retCode closePort(udpPo u);
static retCode setUdpStatus(udpPo f, retCode status);
static retCode udpStatus(udpPo u);

UdpClassRec UdpClass = {
  {
    (classPo) &LockedClass,               /* parent class is locked object */
    "udp",                                /* this is the udp class */
    inheritUDP,                           /* deal with inheritance */
    initUDPClass,                         /* UDP class initializer */
    O_INHERIT_DEF,                        /* UDP object element creation */
    O_INHERIT_DEF,                        /* UDP objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    UdpInit,                              /* initialization of an UDP buffer */
    sizeof(UdpObject),                    /* min size of an io record -- should never use */
    NULL,                                 /* pool of values for this class */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,        /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    udpReadyIn,                           /* readyIn  */
    udpReadyOut,                          /* readyOut  */
    closePort                             /* close  */
  }
};

classPo udpClass = (classPo) &UdpClass;

static void inheritUDP(classPo class, classPo request) {
  UdpClassRec *req = (UdpClassRec *) request;
  UdpClassRec *template = (UdpClassRec *) class;
  logical done = False;

  while (!done) {
    done = True;

    if (req->udpPart.inReady == O_INHERIT_DEF) {
      if (template->udpPart.inReady != O_INHERIT_DEF)
        req->udpPart.inReady = template->udpPart.inReady;
      else
        done = False;
    }

    if (req->udpPart.outReady == O_INHERIT_DEF) {
      if (template->udpPart.outReady != O_INHERIT_DEF)
        req->udpPart.outReady = template->udpPart.outReady;
      else
        done = False;
    }

    if (req->udpPart.close == O_INHERIT_DEF) {
      if (template->udpPart.close != O_INHERIT_DEF)
        req->udpPart.close = template->udpPart.close;
      else
        done = False;
    }

    template = (UdpClassRec *) (template->objectPart.parent);
  }
}

static pthread_once_t udpOnce = PTHREAD_ONCE_INIT;

static void initMutexes(void) {
  initRecursiveMutex(&udpClass->mutex);
}

static void initUDPClass(classPo class, classPo request) {
  pthread_once(&udpOnce, initMutexes);
}

static void UdpInit(objectPo o, va_list *args) {
  udpPo f = O_UDP(o);
  char * name = va_arg(*args, char *);

  lockClass(udpClass);

  uniCpy(f->udp.name, NumberOf(f->udp.name), name);

  int port = va_arg(*args, int);

  f->udp.port = (uint16) port;

  int sock = va_arg(*args, int);
  f->udp.sock = (uint16) sock;

  ioDirection dir = va_arg(*args, ioDirection);
  f->udp.mode = dir;

  ioEncoding enc = va_arg(*args, ioEncoding);
  f->udp.encoding = enc;

  unlockClass(udpClass);
}

udpPo newUDPPort(byte *name, int port, ioDirection dir) {
  int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

  if (sock == INVALID_SOCKET)
    return NULL;
  else {
    /* Set the socket to reuse addresses */
    int one = 1, len = sizeof(int);

    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &one, (socklen_t) len) != 0) {
      close(sock);
      return NULL;
    } else {
      struct sockaddr_in addr;  /* Internet-domain protocol */

      memset((char *) &addr, 0, sizeof(addr));
      addr.sin_family = AF_INET;
      addr.sin_addr.s_addr = htonl(INADDR_ANY);
      addr.sin_port = htons((unsigned short) port);

      if (bind(sock, (struct sockaddr *) &addr, sizeof(addr)) != 0) {
        close(sock);
        return NULL;
      }

      return O_UDP(newObject(udpClass, name, port, sock, dir, rawEncoding));
    }
  }
}

retCode closePort(udpPo u) {
  if (udpStatus(u) == Ok) {
    close(u->udp.sock);
  }
  return Ok;
}

retCode closeUDP(udpPo f) {
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));

  if (--(f->object.refCount) <= 0) {
    return ((UdpClassRec *) f->object.class)->udpPart.close(f);
  }

  unlock(O_LOCKED(o));
  return Ok;
}

char * udpName(udpPo f) {
  return f->udp.name;
}

uint16 udpPortNo(udpPo u) {
  return u->udp.port;
}

/* reading from a UDP Socket */
retCode udpRead(udpPo u, byte *buff, long *blen, char * peer, long peerLen, int *port) {
  assert(isUDPport(O_OBJECT(u)));

  again:
  {
    struct sockaddr_in from;
    socklen_t sock_len = sizeof(from);
    ssize_t nBytes = recvfrom(u->udp.sock, buff, (size_t) *blen, 0, (struct sockaddr *) &from, &sock_len);
    int locErr = errno;

    if (nBytes >= 0) {
      *blen = nBytes;                   // Set the actual length of the datagram

      if (port != NULL)
        *port = ntohs(from.sin_port);
      uniCpy(peer, peerLen, inet_ntoa(from.sin_addr));
      return setUdpStatus(u, Ok);
    } else {
      switch (locErr) {
        case EINTR:            /* Interrupted */
          goto again;

        case 0:              /* this shouldnt happen ... but it does */
        case EWOULDBLOCK:            /* Would have blocked */
          setUdpStatus(u, Ok);
          return Fail;
        default:setUdpStatus(u, Eof);
          return Error;                  /* Something unspecific */
      }
    }
  }
}

retCode udpSend(udpPo u, byte *buff, long blen, char * peer, int port) {
  assert(isUDPport(O_OBJECT(u)));

  byte *cp = buff;
  long nBytes;
  size_t actual = (size_t) (blen * sizeof(byte));
  int16 sock = u->udp.sock;
  struct sockaddr_in serv_addr;
  char * host = getHostname(peer);
  struct in_addr *addr = host != NULL ? getHostIP(host, 0) : NULL;

  if (addr != NULL) {
    /* Set up to send to the remote machine */
    memset((char *) &serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr = *addr;
    serv_addr.sin_port = htons((u_short) port);

    while (actual > 0 &&
           (nBytes = sendto(sock, cp, actual, 0, (struct sockaddr *) &serv_addr, sizeof(serv_addr))) != actual) {
      if (nBytes == SOCKET_ERROR) {
        switch (errno) {
          case EWOULDBLOCK:
          case ENOBUFS:
          case EINTR: {
            while (actual > 0 &&
                   (nBytes = sendto(sock, cp, actual, 0, (struct sockaddr *) &serv_addr, sizeof(serv_addr))) !=
                   actual) {
              if (nBytes == SOCKET_ERROR) {
                return Error;
              }
              cp += nBytes;
              actual -= nBytes;
            }
            return Ok;
          }
          default:return Error;
        }
      } else {
        cp += nBytes;
        actual -= nBytes;
      }
    }
  } else
    return Error;

  return Ok;
}

retCode udpReadyIn(udpPo f) {
  int fno = f->udp.sock;

  if ((f->udp.mode & ioREAD) != 0) {
    fd_set fdin;
    struct timeval period;

    FD_ZERO(&fdin);

    FD_SET(fno, &fdin);

    period.tv_sec = 0;
    period.tv_usec = 0;

    if (select(fno + 1, &fdin, NULL, NULL, &period) > 0)
      return Ok;
    else
      return Fail;
  } else
    return ioErrorMsg(logFile, "%s does not permit read access", f->udp.name);
}

retCode udpReadyOut(udpPo f) {
  int fno = f->udp.sock;

  if ((f->udp.mode & ioWRITE) != 0) {
    fd_set fdin;
    struct timeval period;

    FD_ZERO(&fdin);

    FD_SET(fno, &fdin);

    period.tv_sec = 0;
    period.tv_usec = 0;

    if (select(fno + 1, &fdin, NULL, NULL, &period) > 0)
      return Ok;
    else
      return Fail;
  } else
    return ioErrorMsg(logFile, "%s does not permit write access", f->udp.name);
}

logical isUDPport(objectPo o) {
  return objectHasClass(o, udpClass);
}

retCode setUdpStatus(udpPo f, retCode status) {
  objectPo o = O_OBJECT(f);

  lock(O_LOCKED(o));
  f->udp.status = status;
  unlock(O_LOCKED(o));
  return status;
}

retCode udpStatus(udpPo u) {
  return u->udp.status;
}
