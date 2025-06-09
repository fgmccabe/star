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
  switchProcessState(P, wait_io);
  listen = O_IO(listeningPort(nBuff, (unsigned short) port));
  setProcessRunnable(P);

  if (listen == NULL) {
    pshVal(P, eNOPERM);
    return Abnormal;
  } else {
    pshVal(P, (termPo) allocateIOChnnl(processHeap(P), listen));
    return Normal;
  }
}

ReturnStatus g__accept(processPo P) {
  ioPo listen = ioChannel(C_IO(popVal(P)));
  ioEncoding enc = pickEncoding(integerVal(popVal(P)));
  heapPo h = processHeap(P);

  switchProcessState(P, wait_io);

  ioPo inC, outC;

  switchProcessState(P, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(P);

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

        termPo inChnl = (termPo) allocateIOChnnl(h, inC);
        int root = gcAddRoot(h, &inChnl);

        termPo outChnl = (termPo) allocateIOChnnl(h, outC);
        gcAddRoot(h, &outChnl);

        termPo peer = allocateString(h, peerN, uniStrLen(peerN));
        gcAddRoot(h, &peer);

        termPo peerIP = allocateString(h, peerI, uniStrLen(peerI));
        gcAddRoot(h, &peerIP);

        termPo prt = makeInteger(port);
        gcAddRoot(h, &prt);

        normalPo reslt = allocateTpl(h, 5);

        setArg(reslt, 0, inChnl);
        setArg(reslt, 1, outChnl);
        setArg(reslt, 2, peer);
        setArg(reslt, 3, prt);
        setArg(reslt, 4, peerIP);

        gcReleaseRoot(h, root);
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

  heapPo h = processHeap(P);
  ioEncoding enc = pickEncoding(integerVal(popVal(P)));

  switchProcessState(P, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(host, (int) port, enc, True, &inC, &outC);

  setProcessRunnable(P);

  switch (ret) {
    case Ok: {
      termPo inChnl = (termPo) allocateIOChnnl(h, inC);
      int root = gcAddRoot(h, &inChnl);

      termPo outChnl = (termPo) allocateIOChnnl(h, outC);
      gcAddRoot(h, &outChnl);

      normalPo reslt = allocateTpl(h, 2);

      gcReleaseRoot(h, root);
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
  heapPo h = processHeap(P);
  termPo ipList = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, &ipList);
  gcAddRoot(h, &el);

  for (int i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo) allocateCString(h, ip);
    ipList = (termPo) allocateCons(h, el, ipList);
  }

  gcReleaseRoot(h, root);
  pshVal(P, ipList);
  return Normal;
}

/* Access host name from IP address */
ReturnStatus g__iptohost(processPo P) {
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(popVal(P)), ip, NumberOf(ip));
  heapPo h = processHeap(P);
  char *host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(h, host);
    pshVal(P, Host);
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}
