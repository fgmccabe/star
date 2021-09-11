//
// Created by Francis McCabe on 3/8/18.
//

#include <arith.h>
#include <ioTcp.h>
#include <errorCodes.h>
#include <iochnnlP.h>
#include <chars.h>
#include <arithP.h>
#include <tpl.h>
#include <hosts.h>
#include <cons.h>
#include <globals.h>
#include <consP.h>
#include "netfile.h"

ReturnStatus g__listen(processPo P, ptrPo tos) {
  integer port = integerVal(tos[0]);
  char nBuff[MAXFILELEN];
  ioPo listen;

  strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
  switchProcessState(P, wait_io);
  listen = O_IO(listeningPort(nBuff, (unsigned short) port));
  setProcessRunnable(P);

  if (listen == NULL)
    return liberror(P, "_listen", eNOPERM);
  else {
    return (ReturnStatus) {.ret=Ok,
      .result =(termPo) allocateIOChnnl(processHeap(P), listen)};
  }
}

ReturnStatus g__accept(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  ioPo listen = ioChannel(C_IO(Arg1));
  ioEncoding enc = pickEncoding(integerVal(Arg2));

  switchProcessState(P, wait_io);

  ioPo inC, outC;

  switchProcessState(P, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(P);

  if (listen == NULL)
    return liberror(P, "_accept", eNOPERM);
  else {

    switch (ret) {
      case Ok: {
        int port;
        char pBuff[MAXFILELEN];
        char *peerN = peerName(O_SOCK(inC), &port);
        char *peerI = peerIP(O_SOCK(inC), &port, &pBuff[0], NumberOf(pBuff));

        if (peerN == NULL || peerI == NULL) {
          closeFile(inC);
          closeFile(outC);
          return liberror(P, "_accept", eNOTFND);
        }

        heapPo H = processHeap(P);
        termPo inChnl = (termPo) allocateIOChnnl(H, inC);
        int root = gcAddRoot(H, &inChnl);

        termPo outChnl = (termPo) allocateIOChnnl(H, outC);
        gcAddRoot(H, &outChnl);

        termPo peer = (termPo) allocateChars(H, peerN, uniStrLen(peerN));
        gcAddRoot(H, &peer);

        termPo peerIP = (termPo) allocateChars(H, peerI, uniStrLen(peerI));
        gcAddRoot(H, &peerIP);

        termPo prt = (termPo) allocateInteger(H, port);
        gcAddRoot(H, &prt);

        normalPo reslt = allocateTpl(H, 5);

        setArg(reslt, 0, inChnl);
        setArg(reslt, 1, outChnl);
        setArg(reslt, 2, peer);
        setArg(reslt, 3, prt);
        setArg(reslt, 4, peerIP);

        gcReleaseRoot(H, root);

        return (ReturnStatus) {.ret=Ok, .result =(termPo) reslt};
      }
      default:
        return liberror(P, "_accept", eIOERROR);
    }
  }
}

ReturnStatus g__connect(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  integer port = integerVal(Arg2);

  integer hLen;
  const char *host = charsVal(Arg1, &hLen);

  ioEncoding enc = pickEncoding(integerVal(Arg3));

  switchProcessState(P, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(host, (int) port, enc, True, &inC, &outC);

  setProcessRunnable(P);

  switch (ret) {
    case Ok: {
      heapPo H = processHeap(P);
      termPo inChnl = (termPo) allocateIOChnnl(H, inC);
      int root = gcAddRoot(H, &inChnl);

      termPo outChnl = (termPo) allocateIOChnnl(H, outC);
      gcAddRoot(H, &outChnl);

      normalPo reslt = allocateTpl(H, 2);

      gcReleaseRoot(H, root);

      return (ReturnStatus) {.ret=Ok, .result =(termPo) reslt};
    }
    default:
      logMsg(logFile, "Failed to establish connection: %S", host, hLen);
      return liberror(P, "_connect", eCONNECT);
  }
}

/* Access host name functions */
/* return IP addresses of a host */
ReturnStatus g__hosttoip(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  char host[MAXFILELEN];
  char ip[MAX_SYMB_LEN];
  heapPo H = processHeap(P);

  copyChars2Buff(C_CHARS(Arg1), host, NumberOf(host));

  termPo ipList = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(H, &ipList);
  gcAddRoot(H, &el);

  for (int i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo) allocateCString(H, ip);
    ipList = (termPo) allocateCons(H, el, ipList);
  }

  gcReleaseRoot(H, root);
  return (ReturnStatus) {.ret=Ok, .result =(termPo) ipList};
}

/* Access host name from IP address */
ReturnStatus g__iptohost(processPo P, ptrPo tos) {
  char ip[MAX_SYMB_LEN];
  heapPo H = processHeap(P);

  copyChars2Buff(C_CHARS(tos[0]), ip, NumberOf(ip));

  char *host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(H, host);
    return (ReturnStatus) {.ret=Ok, .result =Host};
  } else
    return liberror(P, "_iptohost", eNOTFND);
}



