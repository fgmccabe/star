/*
  Socket interface library
  (c) 1994-2004 Imperial College, F.G. McCabe  and Fujitsu Labs

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
  
  Contact: <frankmccabe@mac.com>
*/

#include "config.h"		/* Invoke configuration header */
#include "iosockP.h"
#include "hosts.h"
#include "iostr.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> 
#include <netinet/tcp.h> 
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <limits.h>
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

/* Set up the socket class */

static void initSockClass(classPo class,classPo request);
static void SockDestroy(objectPo o);
static void SockInit(objectPo list,va_list *args);

static retCode configureSocket(filePo f, ioConfigOpt mode);

static retCode refillSocket(filePo f);
static retCode sockFlush(ioPo f,long count);
static retCode sockSeek(ioPo f,long count);

SockClassRec SockClass = {
  {
    (classPo)&FileClass,                    /* parent class is file object */
    "sock",                                 /* this is the sock class */
    NULL,				    /* inherit from sock */
    initSockClass,                          /* Sock class initializer */
    O_INHERIT_DEF,                          /* Sock object element creation */
    SockDestroy,                            /* Sock object destruction */
    O_INHERIT_DEF,                          /* erasure */
    SockInit,				/* initialization of a file object */
    sizeof(SockObject),
    NULL,				    /* pool of file values */
    PTHREAD_ONCE_INIT,			    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    NULL
  },
  {
    O_INHERIT_DEF,                        /* inChar  */
    O_INHERIT_DEF,                        /* outChar  */
    O_INHERIT_DEF,                        /* ungetChar  */
    O_INHERIT_DEF,                        /* inBytes  */
    O_INHERIT_DEF,                        /* outBytes  */
    O_INHERIT_DEF,                        /* backByte  */
    O_INHERIT_DEF,                        /* atEof  */
    O_INHERIT_DEF,                        /* readyIn  */
    O_INHERIT_DEF,                        /* readyOut  */
    sockFlush,                            /* flush  */
    sockSeek,                             /* seek  */
    O_INHERIT_DEF                         /* close  */
  },
  {
    configureSocket,                    /* configure a socket */
    refillSocket                        /* refill a socket */
  },
  {
  }
};

classPo sockClass = (classPo)&SockClass;

static void initSockClass(classPo class,classPo request)
{
  assert(request->pool!=NULL);
}


static void SockInit(objectPo o,va_list *args)
{
}


static void SockDestroy(objectPo o)
{
}


/* Socket reading and writing functions */
static retCode refillSocket(filePo f)
{
  sockPo sk = O_SOCK(f);
  int sock = sk->file.fno;

  if(f->file.in_pos>=f->file.in_len){	/* nead to read more input? */
    int len;
    int lerrno;                         /* local copy of errno */
    
    stopAlarm();                        /* Stop the time interrupt */
    len = recv(sock,f->file.in_line,MAXLINE,0);
    lerrno = errno;
    startAlarm();                       /* Restart the timer interrupt */
    
    if(len<0){                          /* something wrong? */
      switch(lerrno){
      case EAGAIN:
	return Fail;			/* No data available */
      case EINTR:
	return Interrupt;		/* We were interrupted */
      default:
        f->file.in_pos = f->file.in_len = 0;
        f->io.status = Eof;             /* we have reach end of file */
	return ioErrorMsg(O_IO(f),"error %s in reading from socket",strerror(lerrno));
      }
    }
    else{
      f->file.in_pos = 0;
      f->file.in_len = len;

      if(len==0)                        /* we have reach end of file */
        f->io.status = Eof;
      else
        f->io.status = Ok;
      return f->io.status;
    }
  }
  else
    return Ok;			/* Already got stuff in there */
}

static retCode sockFlush(ioPo io,long count)
{
  sockPo f = O_SOCK(io);
  int fno = f->file.fno;
  long written;
  long remaining = f->file.out_pos;
  byte *cp = f->file.out_line;
  long writeGap = 0;

  if(count>0 && f->file.out_pos+count<NumberOf(f->file.out_line))
    return Ok;
  
  while(remaining>0 && (written=send(fno,cp,remaining,0))!=remaining){
    if(written==-1){
      switch(errno){
      case 0:                           /* Linux and/or solaris sometimes does this */
	continue;
      case EINTR:
        if(writeGap>0){
          memmove(&f->file.out_line[0],cp,sizeof(byte)*remaining);
          f->file.out_pos=remaining;
        }
        return Interrupt;		/* report an interrupted transfer */
      case EAGAIN:{                     /* we shuffle the remaining buffer to the front */
        if(writeGap>0){
          memmove(&f->file.out_line[0],cp,sizeof(byte)*remaining);
          f->file.out_pos=remaining;
        }
        return Fail;
      }
	
      default:
        return ioErrorMsg(O_IO(f),
			  "Problem %s (%d) in writing to %U",strerror(errno),errno,
                          fileName(O_IO(f)));
      }
    }
    else{
      cp+=written;
      writeGap += written;
      remaining-=written;
    }
  }
  f->file.out_pos = 0;
  f->io.status = Ok;
  return Ok;
}

static retCode sockSeek(ioPo io,long count)
{
  return Fail;                          /* cant seek sockets */
}


static retCode configureSocket(filePo f, ioConfigOpt mode)
{
  int sock = f->file.fno;

  switch(mode){
  case turnOffBlocking:{
    int SokOpt = True;
    int fd_flags=fcntl(sock,F_GETFL,0);

    if(fd_flags==-1)
      return Error;

    fd_flags |= O_NONBLOCK;
    if(fcntl(sock,F_SETFL,fd_flags)==-1)
      return Error;
    /* Set the socket to non-delay mode to avoid buffering problems */
    setsockopt(sock,IPPROTO_TCP,TCP_NODELAY,(char*)&SokOpt,sizeof(SokOpt));
    return Ok;
  }

  case turnOnBlocking:{
    int fd_flags=fcntl(sock,F_GETFL,0);

    if(fd_flags==-1)
      return Error;

    fd_flags &= ~O_NONBLOCK;
    if(fcntl(sock,F_SETFL,fd_flags)==-1)
      return Error;
    return Ok;
  }

  case enableAsynch:{		/* Enable interrupt driven I/O */
#ifdef SYSV
    if(ioctl(sock,I_SETSIG,S_INPUT|S_OUTPUT)>=0)
      return Ok;
    else
      return Error;
#else
    int fd_flags=fcntl(sock,F_GETFL,0);

    if(fd_flags==-1)
      return Error;

    if(fcntl(sock,F_SETOWN,getpid())<0 || fcntl(sock,F_SETFL,fd_flags|O_ASYNC)<0)
      return Error;
    else
      return Ok;
#endif
  }

  case disableAsynch:{		/* We no longer want to receive interrupts */
#ifdef SYSV
    if(ioctl(sock,I_SETSIG,0)>=0)
      return Ok;
    else
      return Error;
#else
    int fd_flags=fcntl(sock,F_GETFL,0);

    if(fd_flags==-1)
      return Error;

    if(fcntl(sock,F_SETFL,fd_flags&~O_ASYNC)<0)
      return Error;
    else
      return Ok;
#endif
  }

  default:
    return Error;			/* Unknown option */
  }
}

sockPo listeningPort(uniChar *name,int port)
{
  int sock = socket(AF_INET,SOCK_STREAM,0);

  if(sock==INVALID_SOCKET)
    return NULL;
  else{
    /* Set the socket to reuse addresses */
    int one=1, len=sizeof(int);

    if(setsockopt(sock,SOL_SOCKET,SO_REUSEADDR,(char*)&one,len)!=0){
      close(sock);
      return NULL;
    }
    else{
      struct sockaddr_in addr;	/* Internet-domain protocol */

      memset((char *)&addr, 0, sizeof(addr));
      addr.sin_family = AF_INET;
      addr.sin_addr.s_addr = htonl(INADDR_ANY);
      addr.sin_port = htons((unsigned short)port);
  
      if(bind(sock,(struct sockaddr*)&addr,sizeof(addr))!=0){
	close(sock);
	return NULL;
      }

      listen(sock,16);   /* we permit up to 16 connections to be backlogged */

      return O_SOCK(newObject(sockClass,name,sock,unknownEncoding,ioREAD));
    }
  }
}

/* acceptConnection allows a connection from a connect socket and returns
   a filePo which can read/write to the remote host
 */
retCode acceptConnection(sockPo listen,ioEncoding encoding,
			 ioPo *inC,ioPo *outC)
{
  struct sockaddr_in cli_addr;
  socklen_t clilen = sizeof(cli_addr);
  int lSock = listen->file.fno;
  int cliSock;

  memset((char *)&cli_addr, 0, sizeof(cli_addr));

  cliSock = accept(lSock,(struct sockaddr*)&cli_addr, &clilen);

  if(cliSock<0){
    switch(errno){
    case EWOULDBLOCK:
      return Fail;
    case EINTR:			/* Interrupted */
      return Interrupt;
    default:
      return ioErrorMsg(O_IO(listen),
			"problem %s (%d) in accepting connection at socket %U",
			strerror(errno),errno,fileName(O_IO(listen)));
    }
  }
  else{
    uniChar cl_name[MAXLINE];
    struct sockaddr_in addr;
    socklen_t addrLen = sizeof(addr);

    getsockname(cliSock,(struct sockaddr*)&addr,&addrLen);

    strMsg(cl_name,NumberOf(cl_name),"%s:%d",inet_ntoa(addr.sin_addr),
           ntohs(addr.sin_port));

    *inC = O_IO(newObject(sockClass,cl_name,cliSock,encoding,ioREAD));
    *outC = O_IO(newObject(sockClass,cl_name,cliSock,encoding,ioWRITE));

    return Ok;
  }
}

/* return the peername of a connection */
uniChar *peerName(sockPo stream,int *port)
{
  struct sockaddr_in cli_addr;
  socklen_t cli_len = sizeof(cli_addr);
  int sock = stream->file.fno;
  int stat = getpeername(sock,(struct sockaddr*)&cli_addr,&cli_len);

  if(stat==0){
    if(port!=NULL)
      *port = ntohs(cli_addr.sin_port);

    return fileName(O_IO(stream));
  }
  else
    return NULL;
}

/* return the peername IP of a connection */
uniChar *peerIP(sockPo stream,int *port,uniChar *buff,long len)
{
  struct sockaddr_in cli_addr;
  socklen_t cli_len = sizeof(cli_addr);
  int sock = stream->file.fno;
  int stat = getpeername(sock,(struct sockaddr*)&cli_addr,&cli_len);

  if(stat==0){
    if(port!=NULL)
      *port = ntohs(cli_addr.sin_port);

    _uni((unsigned char*)inet_ntoa(cli_addr.sin_addr),buff,len);

    return buff;
  }
  else
    return NULL;
}


/* Attempt a connection with a server 
   specified as a pair: hostname(or ip address)/port
*/

static retCode connRemote(uniChar *host,struct in_addr *addr,int port,
			  ioEncoding encoding,logical waitForMe,
			  ioPo *inC,ioPo *outC);

retCode connectRemote(uniChar *where,int port,ioEncoding encoding,
		      logical waitForMe,
		      ioPo *inC,ioPo *outC)
{
  uniChar *host = getHostname(where);
  int i=0;
  struct in_addr *addr = host!=NULL?getHostIP(host,i):NULL;

  while(addr!=NULL){
    retCode ret = connRemote(host,addr,port,encoding,waitForMe,inC,outC);

    if(ret!=Error)
      return ret;
    else{
      i++;
      addr = getHostIP(host,i);
    }
  }

  if(host!=NULL)
    markHostUnavail(host);
  return Error;
}

static retCode connRemote(uniChar *host,struct in_addr *addr,int port,
			  ioEncoding encoding,logical waitForMe,
			  ioPo *inC,ioPo *outC)
{
  int sock;
  struct sockaddr_in serv_addr;

  /* Attempt to establish links to the server */
  memset((char *)&serv_addr, 0, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr = *addr;
  serv_addr.sin_port = htons((u_short)port);
  
  /* Create the socket ... */
  if((sock=socket(AF_INET,SOCK_STREAM,0)) == INVALID_SOCKET){
    return Error;
  }
  else{
    sockPo conn = O_SOCK(newObject(sockClass,host,sock,encoding,ioREAD));
    *inC = O_IO(conn);		/* early return */

    configureIo(O_FILE(conn),(waitForMe?turnOnBlocking:turnOffBlocking));
    conn = O_SOCK(newObject(sockClass,host,sock,encoding,ioWRITE));
    *outC = O_IO(conn);
      
    while(connect(sock,(struct sockaddr *)&serv_addr,sizeof(serv_addr))!=0){
      closeFile(O_IO(conn));
      switch(errno){		/* Unix version */
      case EACCES:
      case EADDRNOTAVAIL:
        return ioErrorMsg(O_IO(conn),"Address %U not available",host);
      case ECONNREFUSED:
        return ioErrorMsg(O_IO(conn),"Connection to %U refused",host);
      case ETIMEDOUT:
        return ioErrorMsg(O_IO(conn),"Connection to %U timed out",host);
      case ENETUNREACH:
        return ioErrorMsg(O_IO(conn),"Network down or %U unreachable",host);
      case EINTR:
	return Interrupt;		/* We were interrupted */
      case EALREADY:
      case EWOULDBLOCK:
      case EINPROGRESS:
        return Fail;
      default:
        return ioErrorMsg(O_IO(conn),"Error %s (%d) in connecting to %U",
			  strerror(errno),errno,host);
      }
    }

    configureIo(O_FILE(conn),(waitForMe?turnOnBlocking:turnOffBlocking));
    
    return Ok;
  }
}

retCode resumeRemoteConnect(sockPo conn,uniChar *host,ioPo *outC)
{
  /* Resume on writing */
  int sock = O_FILE(conn)->file.fno;
  int result;
  socklen_t resultLen = sizeof(result);

  getsockopt(sock,SOL_SOCKET,SO_ERROR,&result,&resultLen);
  if(result == 0) {
    *outC = O_IO(newObject(sockClass,host,sock,fileEncoding(O_FILE(conn)),ioWRITE));
    return Ok;
  }
   else{
    switch(errno){
    case EACCES:
    case EADDRNOTAVAIL:
      closeFile(O_IO(conn));
      return ioErrorMsg(O_IO(conn),"Address %U not available",host);
    case ECONNREFUSED:
      closeFile(O_IO(conn));
      return ioErrorMsg(O_IO(conn),"Connection to %U refused",host);
    case ETIMEDOUT:
      closeFile(O_IO(conn));
      return ioErrorMsg(O_IO(conn),"Connection to %U timed out",host);
    case ENETUNREACH:
      closeFile(O_IO(conn));
      return ioErrorMsg(O_IO(conn),"Network down or %U unreachable",host);
    case EINTR:
      return Interrupt;
    case EALREADY:
    case EWOULDBLOCK:
    case EINPROGRESS:
      return Fail;
    default:
      closeFile(O_IO(conn));
      return ioErrorMsg(O_IO(conn),"Error %s (%d) in connecting to %U",
			strerror(errno),errno,host);
    }
  }
}

