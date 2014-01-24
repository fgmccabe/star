/*
  UDP socket handling functions 
  (c) 1994-2006 Imperial College & F.G. McCabe

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

  Contact: Francis McCabe <frankmccabe@mac.com>
*/
#include "config.h"		/* pick up standard configuration header */
#include "iosockP.h"
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


filePo udpPort(uniChar *name,int port)
{
  int sock = socket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);

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

      return _new_io(name,ioUDP,NULL,NULL,ioREAD|ioWRITE,(void*)sock,
		     NULL,NULL,_closeSocket,NULL,NULL,_configureSocket);
    }
  }
}




/* reading from a UDP Socket */
retCode udpRead(filePo f,byte *buff,long *blen,uniChar *peer,long len,int *port)
{

again:
  {
    struct sockaddr_in from;
    int len = sizeof(from);
    int nBytes = recvfrom((int)fileData(f),buff,*blen,0,(struct sockaddr *)&from,&len);
    int locErr = errno;

    if(nBytes>=0){
      *blen = nBytes;                   // Set the actual length of the datagram
        
      if(port!=NULL)
        *port = ntohs(from.sin_port);
       
      _uni(inet_ntoa(from.sin_addr),peer,len);
      return setBufferStatus(f,Ok);
    }
    else{
      switch(locErr){
      case EINTR:		        /* Interrupted */
	goto again;

      case 0:			        /* this shouldnt happen ... but it does */
      case EWOULDBLOCK:		        /* Would have blocked */
	setBufferStatus(f,Ok);
	return Fail;
      default:
	resetInBuffer(f,0,0);
	setBufferStatus(f,Eof);
	return Error;	                /* Something unspecific */
      }
    }
  }
}

retCode udpSend(filePo f,byte *buff,long blen,uniChar *peer,int port)
{
  assert(fileType(f)==ioUDP);
  
  {
    byte *cp = buff;
    long nBytes;
    long actual=blen*sizeof(byte);
    int sock = (int)fileData(f);
    struct sockaddr_in serv_addr;
    uniChar *host = getHostname(peer);
    struct in_addr *addr = host!=NULL?getHostIP(host,0):NULL;

    if(addr!=NULL){
      /* Set up to send to the remote machine */
      memset((char *)&serv_addr, 0, sizeof(serv_addr));
      serv_addr.sin_family = AF_INET;
      serv_addr.sin_addr = *addr;
      serv_addr.sin_port = htons((u_short)port);
      
      while(actual>0 && (nBytes=sendto(sock,cp,actual,0,(struct sockaddr *)&serv_addr,sizeof(serv_addr)))!=actual){
        if(nBytes==SOCKET_ERROR){
	  switch(errno){
	  case EWOULDBLOCK:
          case ENOBUFS:
	  case EINTR:{
            _configureSocket(f,turnOnBlocking,(void*)sock);    /* so we have to finish */
            
            while(actual>0 && (nBytes=sendto(sock,cp,actual,0,(struct sockaddr *)&serv_addr,sizeof(serv_addr)))!=actual){
              if(nBytes==SOCKET_ERROR){
	        // logMsg(logFile,"Problem %s (%d) in sending %d bytes to %U[%d]\n",
		//        strerror(errno),errno,actual,f->filename,f->client);
		_configureSocket(f,turnOffBlocking,(void*)sock);
	        return Error;
              }
              cp += nBytes;
              actual -= nBytes;
            }
            _configureSocket(f,turnOffBlocking,(void*)sock);
            return Ok;
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
    else
      return Error;
  }
  return Ok;
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

