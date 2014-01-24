/*
  SSL interface library for Go & April
  (c) 1994-2003 Imperial College, F.G. McCabe  and Fujitsu Labs

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
  
  Contact: fgm@fla.fujitsu.com
*/

#include "config.h"		/* Invoke configuration header */
#include "ioP.h"
#include "hosts.h"
#include "ioTcp.h"
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

#include <openssl/ssl.h>        /* The SSL library header*/
#include <openssl/rand.h>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR -1
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

static retCode _configureSSL(filePo f, ioConfigOpt mode,int sock);
static SSL_CTX *sslProfile = NULL;

static void initSSL()
{
  static inited = False;

  if(!inited){
    inited=True;
    SSL_load_error_strings();
    SSL_library_init();
    RAND_screen();

    sslProfile = SSL_CTX_NEW(SSLv23_method);

    if(sslProfile==NULL){
    }

  }
}
    
filePo listeningPort(uniChar *name,int port)
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

      listen(sock,5);

      return _new_io(name,ioFile,unknownEncoding,ioREAD,sock,
		     _refillSSL,0,_closeSSL,
		     _fileInReady,_fileOutReady,_configureSSL);
    }
  }
}

/* acceptConnection allows a connection from a connect socket and returns
   a filePo which can read/write to the remote host
 */
filePo acceptConnection(filePo listen,retCode *res)
{
  struct sockaddr_in cli_addr;
  int clilen = sizeof(cli_addr);
  int lSock = (int)fileData(listen);
  int cliSock;

  memset((char *)&cli_addr, 0, sizeof(cli_addr));

  cliSock = accept(lSock,(struct sockaddr*)&cli_addr, &clilen);

  if(cliSock<0){
    switch(errno){
    case EWOULDBLOCK:
      *res = Fail;
      return NULL;
    case EINTR:			/* Interrupted */
      *res = Eof;
      return NULL;
    default:
      *res=Error;
      return NULL;
    }
  }
  else{
    uniChar cl_name[MAXLINE];
    struct in_addr addr = *(struct in_addr*)&cli_addr;
    strMsg(cl_name,NumberOf(cl_name),"%s:%d",inet_ntoa(addr),ntohs(cli_addr.sin_port));

    *res=Ok;

    return _new_io(cl_name,ioFile,unknownEncoding,ioREAD|ioWRITE,cliSock,
		   _refillSSL,_flushSSL,_closeSSL,
		   _fileInReady,_fileOutReady,_configureSSL);
  }
}

retCode isUDPport(filePo f)
{
  if(f!=NULL){
    if(fileType(f)==ioUDP)
      return Ok;
    else
      return Fail;
  }
  else
    return Error;
}

/* return the peername of a connection */
uniChar *sslName(filePo stream,int *port)
{
  struct sockaddr_in cli_addr;
  int cli_len = sizeof(cli_addr);
  int sock = (int)fileData(stream);
  int stat = getpeername(sock,(struct sockaddr*)&cli_addr,&cli_len);

  if(stat==0){
    if(port!=NULL)
      *port = ntohs(cli_addr.sin_port);

    return fileName(stream);
  }
  else
    return NULL;
}

/* return the peername IP of a connection */
uniChar *sslIP(filePo stream,int *port,uniChar *buff,long len)
{
  struct sockaddr_in cli_addr;
  int cli_len = sizeof(cli_addr);
  int sock = (int)fileData(stream);
  int stat = getpeername(sock,(struct sockaddr*)&cli_addr,&cli_len);

  if(stat==0){
    if(port!=NULL)
      *port = ntohs(cli_addr.sin_port);

    _uni(inet_ntoa(cli_addr.sin_addr),buff,len);

    return buff;
  }
  else
    return NULL;
}

static void setConnectRetMsg(uniChar *retmsg,long retlen,uniChar *host)
{
  switch(errno){		/* Unix version */
  case EACCES:
  case EADDRNOTAVAIL:
    if(retmsg!=NULL)
      strMsg(retmsg,retlen,"Address %U not available",host);
    return;
  case ECONNREFUSED:
    if(retmsg!=NULL)
      strMsg(retmsg,retlen,"Connection to %U refused",host);
    return;
  case ETIMEDOUT:
    if(retmsg!=NULL)
      strMsg(retmsg,retlen,"Connection to %U timed out",host);
    return;
  case ENETUNREACH:
    if(retmsg!=NULL)
      strMsg(retmsg,retlen,"Network down or %U unreachable",host);
    return;
  default:
    strMsg(retmsg,retlen,"Connect error (%s): %s", host, strerror(errno));
    return;
  }
}

/* Attempt a connection with a server 
   specified as a pair: hostname(or ip address)/port
*/

filePo connectRemote(uniChar *where,int port,retCode *ret,
		     uniChar *retmsg,long retlen,ioEncoding encoding,logical waitForMe)
{
  int sock;
  struct sockaddr_in serv_addr;
  uniChar *host = getHostname(where);
  struct in_addr *addr = host!=NULL?getHostIP(host):NULL;

  initSSL();

  if(addr!=NULL){
    /* Attempt to establish links to the server */
    memset((char *)&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr = *addr;
    serv_addr.sin_port = htons((u_short)port);
  
    /* Create the socket ... */
    if((sock=socket(AF_INET,SOCK_STREAM,0)) == INVALID_SOCKET){
      *ret = Error;
      if(retmsg!=NULL)
	strMsg(retmsg,retlen,"invalid socket");
      return NULL;
    }
    else{
      filePo conn = _new_io(host,ioFile,encoding,ioREAD|ioWRITE,sock,      // default to UTF8 encoding
		     _refillSSL,_flushSSL,_closeSSL,
		     _fileInReady,_fileOutReady,_configureSSL);
      configureIo(conn,(waitForMe?turnOnBlocking:turnOffBlocking));
      
      while(connect(sock,(struct sockaddr *)&serv_addr,sizeof(serv_addr))!=0){
	switch(errno){		/* Unix version */
	case EACCES:
	case EADDRNOTAVAIL:
	  if(retmsg!=NULL)
	    strMsg(retmsg,retlen,"Address %U not available",host);
	  markHostUnavail(host);
	  closeFile(conn);
	  *ret = Error;
	  return NULL;
	case ECONNREFUSED:
	  if(retmsg!=NULL)
	    strMsg(retmsg,retlen,"Connection to %U refused",host);
	  markHostUnavail(host);
	  closeFile(conn);
	  *ret = Error;
	  return NULL;
	case ETIMEDOUT:
	  if(retmsg!=NULL)
	    strMsg(retmsg,retlen,"Connection to %U timed out",host);
	  markHostUnavail(host);
	  closeFile(conn);
	  *ret = Error;
	  return NULL;
	case ENETUNREACH:
	  if(retmsg!=NULL)
	    strMsg(retmsg,retlen,"Network down or %U unreachable",host);
	  markHostUnavail(host);
	  closeFile(conn);
	  *ret = Error;
	  return NULL;
	case EALREADY:
	case EINTR:
	case EWOULDBLOCK:
	case EINPROGRESS:
	  *ret = Fail;
	  return conn;
	default:
	  if(retmsg!=NULL)
	    strMsg(retmsg,retlen,"Connection to %U refused",host);
	  markHostUnavail(host);
	  closeFile(conn);
	  *ret = Error;
	  return NULL;
	}
      }

      *ret = Ok;
      return conn;
    }
  }
  else{
    if(retmsg!=NULL)
      strMsg(retmsg,retlen,"cant resolve host %U",where);
    *ret = Error;
    return NULL;
  }
}

filePo resumeRemoteConnect(filePo socketPo,retCode *ret,uniChar *where,uniChar *retmsg,long retlen)
{
  /* Resume on writing */
  int sock = (int)fileData(socketPo);
  int result, resultLen = sizeof(result);

  getsockopt(sock,SOL_SOCKET,SO_ERROR,&result,&resultLen);
  if(result == 0) {
    *ret = Ok;
    return socketPo;
  }
  else{
    if(retmsg!=NULL)
      setConnectRetMsg(retmsg,retlen,getHostname(where));
    
    closeFile(socketPo);
    *ret = Error;
    return NULL;
  }
}

/* SSL reading and writing functions */
static retCode _refillSSL(filePo f,int sock)
{
again:
  if(emptyInBuffer(f)){
    char buff[MAXLINE];
    int nBytes = recv(sock,buff,NumberOf(buff),0);
    int locErr = errno;

    if(nBytes>0)
      return decodeBuffer(f,buff,nBytes); /* encode into unicode */
    else if(nBytes==0){		/* End of file */
      resetInBuffer(f,0,0);
      return setBufferStatus(f,Eof);	/* We have reached end of file */
    }
    else{
      switch(locErr){
      case EINTR:		/* Interrupted */
	goto again;

      case 0:			/* this shouldnt happen ... but it does */
      case EWOULDBLOCK:		/* Would have blocked */
	resetInBuffer(f,0,0);
	setBufferStatus(f,Ok);
	return Fail;
      default:
	resetInBuffer(f,0,0);
	setBufferStatus(f,Ok);
	return Error;	/* Something unspecific */
      }
    }
  }
  else
    return Ok;
}

static retCode _flushSSL(filePo f,int sock)
{
  if(!emptyOutBuffer(f)){
    long len = ioBufferOutPos(f)*3;
    char buff[len];
    long actual = encodeBuffer(f,buff,len);
    char *cp = buff;
    long nBytes;

    while(actual>0 && (nBytes=send(sock,cp,actual,0))!=actual){
      if(nBytes==SOCKET_ERROR){
	switch(errno){
	case EWOULDBLOCK:
        case ENOBUFS:
	case EINTR:{
	  if(cp!=buff){         /* we were able to write something out ... */
            _configureSSL(f,turnOnBlocking,sock);    /* so we have to finish */
            
            while(actual>0 && (nBytes=send(sock,cp,actual,0))!=actual){
              if(nBytes==SOCKET_ERROR){
	        // logMsg(logFile,"Problem %s (%d) in sending %d bytes to %U[%d]\n",
		//        strerror(errno),errno,actual,f->filename,f->client);
		_configureSSL(f,turnOffBlocking,sock);
	        return Error;
              }
              cp += nBytes;
              actual -= nBytes;
            }
            _configureSSL(f,turnOffBlocking,sock);
            resetOutBuffer(f,0);
            return Ok;
	  }
	  else
	    return Fail;
        }
	default:
	  // logMsg(logFile,"Problem %s (%d) in sending %d bytes to %U[%d]\n",
	  //	 strerror(errno),errno,actual,f->filename,f->client);
	  return Error;
	}
      }
      else{
	cp+=nBytes;
	actual-=nBytes;
      }
    }
  }
  resetOutBuffer(f,0);
  return Ok;
}

static retCode _closeSSL(filePo sk,int sock)
{
  if(close(sock)==-1)
    return Error;
  else
    return Ok;
}

static retCode _configureSSL(filePo f, ioConfigOpt mode,int sock)
{
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

