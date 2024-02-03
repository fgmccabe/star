/*
  SSL interface library for Go
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "fileP.h"
#include "hosts.h"
#include "iosockP.h"
#include "formio.h"
#include <string.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>

#ifdef SYSV
#include <stropts.h>
#endif

#include <errno.h>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR -1
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

static retCode sockSeek(filePo io, integer count);
static retCode sockFill(filePo f);
static retCode sockFlush(filePo io);
static retCode asyncSockFill(filePo f);
static retCode asyncSockFlush(filePo io);

FileClassRec SocketClass = {
  .objectPart={
    .parent = (classPo) &IoClass,                   /* parent class is io object */
    .className = "socket",                  /* this is the socket class */
    .classInit = initFileClass,                        /* File class initializer, phase I */
    .classInherit = O_INHERIT_DEF,
    .create = O_INHERIT_DEF,                        /* File object element creation */
    .destroy = NULL,                                 /* File objectdestruction */
    .erase = O_INHERIT_DEF,                        /* erasure */
    .init = fileInit,                             /* initialization of a file object */
    .size = sizeof(FileObject),                   /* size of a file object */
    .hashCode = O_INHERIT_DEF,                        // Hashcode for files
    .equality = O_INHERIT_DEF,                        // Equality for files
    .pool = NULL,                                 /* pool of file values */
    .inited = PTHREAD_ONCE_INIT,                    /* not yet initialized */
    .mutex = PTHREAD_MUTEX_INITIALIZER
  },
  .lockPart={},
  .ioPart={
    .read = fileInBytes,                        /* inByte  */
    .write = fileOutBytes,                       /* outBytes  */
    .backByte = fileBackByte,                       //  put a byte back in the buffer
    .inputReady = fileInputReady,
    .outputReady = fileOutputReady,
    .isAtEof = fileAtEof,                          //  Are we at end of file?
    .close = fileClose                           //  close
  },
  .filePart={
    .seek = sockSeek,
    .filler = sockFill,
    .asyncFill = asyncSockFill,
    .flush = sockFlush,
    .asyncFlush = asyncSockFlush       //  refill the buffer if needed
  }
};

classPo sockClass = (classPo) &SocketClass;

sockPo listeningPort(char *name, uint16 port) {
  int sock = socket(AF_INET, SOCK_STREAM, 0);

  if (sock == INVALID_SOCKET)
    return NULL;
  else {
    /* Set the socket to reuse addresses */
    int one = 1, len = sizeof(int);

    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &one, (socklen_t) len) != 0) {
      close(sock);
      return NULL;
    } else {
      struct sockaddr_in addr;  //  Internet-domain protocol

      memset((char *) &addr, 0, sizeof(addr));
      addr.sin_family = AF_INET;
      addr.sin_addr.s_addr = htonl(INADDR_ANY);
      addr.sin_port = htons(port);

      if (bind(sock, (struct sockaddr *) &addr, sizeof(addr)) != 0) {
        close(sock);
        return NULL;
      }

      listen(sock, 5);

      return O_SOCK(newObject(sockClass, name, sock, unknownEncoding, ioREAD));
    }
  }
}

/* acceptConnection allows a connection from a connect socket and returns
   a filePo which can read/write to the remote host
 */
retCode acceptConnection(sockPo listen, ioEncoding encoding, ioPo *inC, ioPo *outC) {
  struct sockaddr_in cli_addr;
  socklen_t clilen = sizeof(cli_addr);
  int lSock = fileNumber(O_FILE(listen));
  int cliSock;

  memset((char *) &cli_addr, 0, sizeof(cli_addr));

  cliSock = accept(lSock, (struct sockaddr *) &cli_addr, &clilen);

  if (cliSock < 0) {
    switch (errno) {
      case EWOULDBLOCK:
        return Fail;
      case EINTR:      //  Interrupted
        return Eof;
      default:
        return Error;
    }
  } else {
    char cl_name[MAXLINE];
    struct in_addr addr = *(struct in_addr *) &cli_addr;
    strMsg(cl_name, NumberOf(cl_name), "%s:%d", inet_ntoa(addr), ntohs(cli_addr.sin_port));

    *inC = O_IO(newObject(sockClass, cl_name, cliSock, encoding, ioREAD));
    *outC = O_IO(newObject(sockClass, cl_name, cliSock, encoding, ioWRITE));

    return Ok;
  }
}

/* Attempt a connection with a server 
   specified as a pair: hostname(or ip address)/port
*/

retCode connectRemote(const char *where, int port,
                      ioEncoding encoding, logical waitForMe,
                      ioPo *inC, ioPo *outC) {
  int sock;
  struct sockaddr_in serv_addr;
  char *host = getHostname(where);
  struct in_addr *addr = host != NULL ? getHostIP(host, 0) : NULL;

  if (addr != NULL) {
    /* Attempt to establish links to the server */
    memset((char *) &serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr = *addr;
    serv_addr.sin_port = htons((u_short) port);

    /* Create the socket ... */
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
      return Error;
    } else {
      ioPo conn = O_IO(newObject(sockClass, host, sock, encoding, ioREAD | ioWRITE));

      while (connect(sock, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) != 0) {
        switch (errno) {    //  Unix version
          case EACCES:
          case EADDRNOTAVAIL:
            outMsg(logFile, "Address %s not available", host);
            markHostUnavail(host);
            closeIo(O_IO(conn));
            return Error;
          case ECONNREFUSED:
            outMsg(logFile, "Connection to %s refused", host);
            markHostUnavail(host);
            closeIo(O_IO(conn));
            return Error;
          case ETIMEDOUT:
            outMsg(logFile, "Connection to %s timed out", host);
            markHostUnavail(host);
            closeIo(O_IO(conn));
            return Error;
          case ENETUNREACH:
            outMsg(logFile, "Network down or %s unreachable", host);
            markHostUnavail(host);
            closeIo(O_IO(conn));
            return Error;
          case EALREADY:
          case EINTR:
          case EWOULDBLOCK:
          case EINPROGRESS:
            closeIo(O_IO(conn));
            return Fail;
          default:
            outMsg(logFile, "Connection to %s refused", host);
            markHostUnavail(host);
            closeIo(O_IO(conn));
            return Error;
        }
      }

      *inC = conn;
      *outC = conn;
      return Ok;
    }
  } else {
    outMsg(logFile, "cant resolve host %s", where);
    return Error;
  }
}

/* Socket reading and writing functions */
static retCode sockFill(filePo f) {
  again:
  if (f->file.in_pos >= f->file.in_len) {
    ssize_t nBytes = recv(f->file.fno, f->file.in_line, NumberOf(f->file.in_line), 0);
    int locErr = errno;

    if (nBytes > 0) {
      f->file.in_pos = 0;
      f->file.in_len = (int16) nBytes;
      return Ok;
    } else if (nBytes == 0) {    //  End of file
      f->file.in_pos = 0;
      f->file.in_len = 0;
      f->io.status = Eof;
      return Eof;
    } else {
      switch (locErr) {
        case EINTR:    //  Interrupted
          goto again;

        case 0:      //  this shouldnt happen ... but it does
        case EWOULDBLOCK:    //  Would have blocked
          f->file.in_pos = 0;
          f->file.in_len = 0;
          setBufferStatus(O_IO(f), Ok);
          return Fail;
        default:
          f->file.in_pos = 0;
          f->file.in_len = 0;
          setBufferStatus(O_IO(f), Ok);
          return Error;  //  Something unspecific
      }
    }
  } else
    return Ok;
}

retCode asyncSockFill(filePo f) {
  return Error;
}

static retCode sockFlush(filePo f) {
  if (f->file.out_pos < NumberOf(f->file.out_line))
    return Ok; // Nothing to do, we have at least count bytes left in the buffer

  if (f->file.out_pos > 0) {
    size_t actual = (size_t) f->file.out_pos;
    int sock = f->file.fno;
    long nBytes;
    byte *buffer = f->file.out_line;
    byte *cp = buffer;

    while (actual > 0 && (nBytes = send(sock, cp, actual, 0)) != actual) {
      if (nBytes == SOCKET_ERROR) {
        switch (errno) {
          case EWOULDBLOCK:
          case ENOBUFS:
          case EINTR: {
            if (cp != buffer) {                     //  we were able to write something out ...
              while (actual > 0 && (nBytes = send(sock, cp, actual, 0)) != actual) {
                if (nBytes == SOCKET_ERROR) {
                  // logMsg(logFile,"Problem %s (%d) in sending %d bytes to %U[%d]\n",
                  //        strerror(errno),errno,actual,f->filename,f->client);
                  return Error;
                }
                cp += nBytes;
                actual -= nBytes;
              }
              f->file.out_pos = 0;
              return Ok;
            } else
              return Fail;
          }
          default:
            // logMsg(logFile,"Problem %s (%d) in sending %d bytes to %U[%d]\n",
            //	 strerror(errno),errno,actual,f->filename,f->client);
            return Error;
        }
      } else {
        cp += nBytes;
        actual -= nBytes;
      }
    }
  }
  f->file.out_pos = 0;
  return Ok;
}

retCode asyncSockFlush(filePo io) {
  return Error;
}

static retCode sockSeek(filePo io, integer count) {
  outMsg(logFile, "seek not implemented on sockets");
  return Error;
}

/* return the peername of a connection */
char *peerName(sockPo stream, int *port) {
  struct sockaddr_in cli_addr;
  socklen_t cli_len = sizeof(cli_addr);
  int sock = stream->file.fno;
  int stat = getpeername(sock, (struct sockaddr *) &cli_addr, &cli_len);

  if (stat == 0) {
    if (port != NULL)
      *port = ntohs(cli_addr.sin_port);

    return fileName(O_IO(stream));
  } else
    return NULL;
}

/* return the peername IP of a connection */
char *peerIP(sockPo stream, int *port, char *buff, long len) {
  struct sockaddr_in cli_addr;
  socklen_t cli_len = sizeof(cli_addr);
  int sock = stream->file.fno;
  int stat = getpeername(sock, (struct sockaddr *) &cli_addr, &cli_len);

  if (stat == 0) {
    if (port != NULL)
      *port = ntohs(cli_addr.sin_port);

    strncpy(buff, inet_ntoa(cli_addr.sin_addr), len);

    return buff;
  } else
    return NULL;
}

