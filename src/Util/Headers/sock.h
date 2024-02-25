/* 
   I/O handling library, TCP/IP header
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
*/ 

#ifndef _IO_TCP_LIB_H_
#define _IO_TCP_LIB_H_

#include "config.h"
#include "file.h"

typedef struct sock_object_ *sockPo;

sockPo listeningPort(char * name, uint16 port);
retCode acceptConnection(sockPo listen,ioEncoding encoding,
			 ioPo *inC,ioPo *outC);

char * peerName(sockPo stream,int *port);
char * peerIP(sockPo stream,int *port,char * buff,long len);
retCode connectRemote(const char *where, int port,
                      ioEncoding encoding, logical waitForMe,
                      ioPo *inC, ioPo *outC);

extern classPo sockClass;

#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_SOCK(c) ((sockPo)(checkCast((c),sockClass)))
#else
#define O_SOCK(c) ((sockPo)(c))
#endif


#endif

