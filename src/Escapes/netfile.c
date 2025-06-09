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

ReturnStatus g__listen(processPo P) {
  integer port = integerVal(popVal(P));
  char nBuff[MAXFILELEN];
  ioPo listen;

  strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
  switchProcessState(currentProcess, wait_io);
  listen = O_IO(listeningPort(nBuff, (unsigned short) port));
  setProcessRunnable(currentProcess);

  if (listen == NULL) {
    pshVal(P, eNOPERM);
    return Abnormal;
  } else {
    pshVal(P, (termPo) allocateIOChnnl(currentHeap, listen));
    return Normal;
  }
}

ReturnStatus g__accept(processPo P) {
  ioPo listen = ioChannel(C_IO(popVal(P)));
  ioEncoding enc = pickEncoding(integerVal(popVal(P)));

  switchProcessState(currentProcess, wait_io);

  ioPo inC, outC;

  switchProcessState(currentProcess, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(currentProcess);

  if (listen == NULL) {
    pshVal(P, eNOPERM);
    return Abnormal;
  } else {
    switch (ret) {
      case Ok: {
        int port;
        char pBuff[MAXFILELEN];
        char *peerN = peerName(O_SOCK(inC), &port);
        char *peerI = peerIP(O_SOCK(inC), &port, &pBuff[0], NumberOf(pBuff));

        if (peerN == NULL || peerI == NULL) {
          closeIo(inC);
          closeIo(outC);
          pshVal(P, eNOTFND);
          return Abnormal;
        }

        termPo inChnl = (termPo) allocateIOChnnl(currentHeap, inC);
        int root = gcAddRoot(currentHeap, &inChnl);

        termPo outChnl = (termPo) allocateIOChnnl(currentHeap, outC);
        gcAddRoot(currentHeap, &outChnl);

        termPo peer = allocateString(currentHeap, peerN, uniStrLen(peerN));
        gcAddRoot(currentHeap, &peer);

        termPo peerIP = allocateString(currentHeap, peerI, uniStrLen(peerI));
        gcAddRoot(currentHeap, &peerIP);

        termPo prt = makeInteger(port);
        gcAddRoot(currentHeap, &prt);

        normalPo reslt = allocateTpl(currentHeap, 5);

        setArg(reslt, 0, inChnl);
        setArg(reslt, 1, outChnl);
        setArg(reslt, 2, peer);
        setArg(reslt, 3, prt);
        setArg(reslt, 4, peerIP);

        gcReleaseRoot(currentHeap, root);
        pshVal(P, (termPo) reslt);
        return Normal;
      }
      default:
        pshVal(P, eIOERROR);
        return Abnormal;
    }
  }
}

ReturnStatus g__connect(processPo P) {
  integer hLen;
  const char *host = strVal(popVal(P), &hLen);
  integer port = integerVal(popVal(P));

  ioEncoding enc = pickEncoding(integerVal(popVal(P)));

  switchProcessState(currentProcess, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(host, (int) port, enc, True, &inC, &outC);

  setProcessRunnable(currentProcess);

  switch (ret) {
    case Ok: {
      termPo inChnl = (termPo) allocateIOChnnl(currentHeap, inC);
      int root = gcAddRoot(currentHeap, &inChnl);

      termPo outChnl = (termPo) allocateIOChnnl(currentHeap, outC);
      gcAddRoot(currentHeap, &outChnl);

      normalPo reslt = allocateTpl(currentHeap, 2);

      gcReleaseRoot(currentHeap, root);
      pshVal(P, (termPo) reslt);
      return Normal;
    }
    default:
      logMsg(logFile, "Failed to establish connection: %S", host, hLen);
      pshVal(P, eCONNECT);
      return Abnormal;
  }
}

/* Access host name functions */
/* return IP addresses of a host */
ReturnStatus g__hosttoip(processPo P) {
  char host[MAXFILELEN];
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(popVal(P)), host, NumberOf(host));

  termPo ipList = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(currentHeap, &ipList);
  gcAddRoot(currentHeap, &el);

  for (int i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo) allocateCString(currentHeap, ip);
    ipList = (termPo) allocateCons(currentHeap, el, ipList);
  }

  gcReleaseRoot(currentHeap, root);
  pshVal(P, ipList);
  return Normal;
}

/* Access host name from IP address */
ReturnStatus g__iptohost(processPo P) {
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(popVal(P)), ip, NumberOf(ip));

  char *host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(currentHeap, host);
    pshVal(P,Host);
    return Normal;
  } else{
    pshVal(P,eNOTFND);
    return Abnormal;
  }
}
