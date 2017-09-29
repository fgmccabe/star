//
// Created by Francis McCabe on 2/19/17.
//

#ifndef LANDO_UDPP_H
#define LANDO_UDPP_H

#include "udp.h"
#include "ioP.h"

typedef retCode (*udpProc)(udpPo f);

typedef struct {
  udpProc inReady;                      /* Called to determine if udp port has input */
  udpProc outReady;                     /* Called to determine if udp port can output */
  udpProc close;                        /* Called when udp port is to be closed */
} UdpClassPartRec;

typedef struct _udp_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;               /* The managed part of the sock */
  UdpClassPartRec udpPart;
} UdpClassRec;

extern UdpClassRec UdpClass; /* the standard pointer to a socket class record */

typedef struct _udp_part_ {
  uint16 port;
  uint16 sock;
  char name[MAXFILELEN];                /* File name */
  retCode status;                       /* current status of the io object */
  ioDirection mode;                     /* Mode that file is opened for */
  ioEncoding encoding;			            /* What is the mode for string encoding */
} UdpPart;

typedef struct _udp_object_ {
  ObjectRec object;                     /* object level of the io structure */
  LockedRecord locked;                   /* The managed part of the socket */
  UdpPart udp;                          /* UDP level of file object */
} UdpObject;

#endif //LANDO_UDPP_H
