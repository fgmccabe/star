//
// Created by Francis McCabe on 3/8/18.
//

#include <arith.h>
#include <sock.h>
#include <errorCodes.h>
#include <iochnnlP.h>
#include <strings.h>
#include <tpl.h>
#include <hosts.h>
#include <cons.h>
#include <globals.h>
#include <consP.h>
#include "netfile.h"

ValueReturn s__listen(enginePo P, integer port) {
  char nBuff[MAXFILELEN];
  ioPo listen;

  strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
  switchProcessState(P, wait_io);
  listen = O_IO(listeningPort(nBuff, (unsigned short) port));
  setProcessRunnable(P);

  if (listen == NULL) {
    return abnormalReturn(eNOPERM);
  }
  else {
    return normalReturn((termPo) allocateIOChnnl(listen));
  }
}

ValueReturn s__accept(enginePo P, ioPo listen, ioEncoding enc) {
  switchProcessState(P, wait_io);

  ioPo inC, outC;

  switchProcessState(P, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(P);

  if (listen == NULL) {
    return abnormalReturn(eNOPERM);
  }
  else {
    switch (ret) {
    case Ok: {
      int port;
      char pBuff[MAXFILELEN];
      char* peerN = peerName(O_SOCK(inC), &port);
      char* peerI = peerIP(O_SOCK(inC), &port, &pBuff[0], NumberOf(pBuff));

      if (peerN == NULL || peerI == NULL) {
        closeIo(inC);
        closeIo(outC);
        return abnormalReturn(eNOTFND);
      }

      termPo inChnl = (termPo)allocateIOChnnl(inC);
      int root = gcAddRoot(&inChnl);

      termPo outChnl = (termPo)allocateIOChnnl(outC);
      gcAddRoot(&outChnl);

      termPo peer = allocateString(peerN, uniStrLen(peerN));
      gcAddRoot(&peer);

      termPo peerIP = allocateString(peerI, uniStrLen(peerI));
      gcAddRoot(&peerIP);

      termPo prt = makeInteger(port);
      gcAddRoot(&prt);

      normalPo reslt = allocateTpl(5);

      setArg(reslt, 0, inChnl);
      setArg(reslt, 1, outChnl);
      setArg(reslt, 2, peer);
      setArg(reslt, 3, prt);
      setArg(reslt, 4, peerIP);

      gcReleaseRoot(root);
      return normalReturn((termPo)reslt);
    }
    default:
      return abnormalReturn(eIOERROR);
    }
  }
}

ValueReturn s__connect(enginePo P, termPo host, int port, ioEncoding enc) {
  integer hLen;
  const char* hostName = strVal(host, &hLen);

  switchProcessState(P, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(hostName, port, enc, True, &inC, &outC);

  setProcessRunnable(P);

  switch (ret) {
  case Ok: {
    termPo inChnl = (termPo)allocateIOChnnl(inC);
    int root = gcAddRoot(&inChnl);

    termPo outChnl = (termPo)allocateIOChnnl(outC);
    gcAddRoot(&outChnl);

    normalPo reslt = allocateTpl(2);

    gcReleaseRoot(root);
    return normalReturn((termPo)reslt);
  }
  default:
    logMsg(logFile, "Failed to establish connection: %S", host, hLen);
    return abnormalReturn(eCONNECT);
  }
}

/* Access host name functions */
/* return IP addresses of a host */
ValueReturn s__hosttoip(enginePo P, termPo hostTerm) {
  char hostName[MAXFILELEN];
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(hostTerm), hostName, NumberOf(hostName));
  termPo ipList = (termPo)nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(&ipList);
  gcAddRoot(&el);

  for (int i = 0; getNthHostIP(hostName, (unsigned)i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo)allocateCString(ip);
    ipList = (termPo)allocateCons(el, ipList);
  }

  gcReleaseRoot(root);
  return normalReturn((termPo)ipList);
}

/* Access host name from IP address */
ValueReturn s__iptohost(enginePo P, termPo hostTerm) {
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(hostTerm), ip, NumberOf(ip));
  char* host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(host);
    return normalReturn(Host);
  }
  else {
    return abnormalReturn(eNOTFND);
  }
}
