//
// Created by Francis McCabe on 2/19/17.
//

#ifndef LANDO_UDP_H
#define LANDO_UDP_H

#include "config.h"
#include "file.h"

typedef struct _udp_object_ *udpPo;

udpPo newUDPPort(byte *name, int port, ioDirection dir);
retCode udpRead(udpPo u,byte *msg,long *blen,char * peer,long len,int *port);
retCode udpSend(udpPo u,byte *msg,long blen,char * peer,int port);
char * udpName(udpPo f);
uint16 udpPortNo(udpPo u);
retCode closeUDP(udpPo u);

extern classPo udpClass;

logical isUDPport(objectPo o);

#ifdef VERIFY_OBJECT
extern objectPo checkCast(void *c,classPo class);

#define O_UDP(c) ((udpPo)(checkCast((c),udpClass)))
#else
#define O_UDP(c) ((udpPo)(c))
#endif


#endif //LANDO_UDP_H
