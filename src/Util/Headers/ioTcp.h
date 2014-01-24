/* 
   I/O handling library, TCP/IP header
   (c) 1994-2006 F.G. McCabe

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

#ifndef _IO_TCP_LIB_H_
#define _IO_TCP_LIB_H_

#include "config.h"
#include "file.h"

typedef struct _sock_object_ *sockPo;

sockPo listeningPort(uniChar *name,int port);
retCode acceptConnection(sockPo listen,ioEncoding encoding,
			 ioPo *inC,ioPo *outC);

uniChar *peerName(sockPo stream,int *port);
uniChar *peerIP(sockPo stream,int *port,uniChar *buff,long len);
retCode connectRemote(uniChar *where,int port,
		      ioEncoding encoding,logical waitForMe,
		      ioPo *inC,ioPo *outC);
retCode resumeRemoteConnect(sockPo socketPo,uniChar *where,ioPo *outC);
sockPo udpPort(uniChar *name,int port);
retCode udpRead(sockPo f,byte *msg,long *blen,uniChar *peer,long len,int *port);
retCode udpSend(sockPo f,byte *msg,long blen,uniChar *peer,int port);


#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_SOCK(c) ((sockPo)(checkCast((c),sockClass)))
#else
#define O_SOCK(c) ((sockPo)(c))
#endif


#endif

