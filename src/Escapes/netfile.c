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
  } else {
    return normalReturn((termPo) allocateIOChnnl(processHeap(P), listen));
  }
}

ReturnStatus g__listen(enginePo P) {
  integer port = integerVal(popVal(P));

  ValueReturn ret = s__listen(P, port);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__accept(enginePo P, ioPo listen, ioEncoding enc) {
  heapPo h = processHeap(P);

  switchProcessState(P, wait_io);

  ioPo inC, outC;

  switchProcessState(P, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(P);

  if (listen == NULL) {
    return abnormalReturn(eNOPERM);
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
          return abnormalReturn(eNOTFND);
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
        return normalReturn((termPo)reslt);
      }
      default:
        return abnormalReturn(eIOERROR);
    }
  }
}

ReturnStatus g__accept(enginePo P) {
  ioPo listen = ioChannel(C_IO(popVal(P)));
  ioEncoding enc = pickEncoding(integerVal(popVal(P)));

  ValueReturn ret = s__accept(P, listen, enc);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__connect(enginePo P, termPo host, int port, ioEncoding enc) {
  integer hLen;
  const char *hostName = strVal(host, &hLen);

  heapPo h = processHeap(P);

  switchProcessState(P, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(hostName, port, enc, True, &inC, &outC);

  setProcessRunnable(P);

  switch (ret) {
    case Ok: {
      termPo inChnl = (termPo) allocateIOChnnl(h, inC);
      int root = gcAddRoot(h, &inChnl);

      termPo outChnl = (termPo) allocateIOChnnl(h, outC);
      gcAddRoot(h, &outChnl);

      normalPo reslt = allocateTpl(h, 2);

      gcReleaseRoot(h, root);
      return normalReturn((termPo)reslt);
    }
    default:
      logMsg(logFile, "Failed to establish connection: %S", host, hLen);
      return abnormalReturn(eCONNECT);
  }
}

ReturnStatus g__connect(enginePo P) {
  termPo host = popVal(P);
  int port = (int) integerVal(popVal(P));
  ioEncoding enc = pickEncoding(integerVal(popVal(P)));

  ValueReturn ret = s__connect(P, host, port, enc);
  pshVal(P, ret.value);
  return ret.status;
}

/* Access host name functions */
/* return IP addresses of a host */
ValueReturn s__hosttoip(enginePo P, termPo hostTerm) {
  char hostName[MAXFILELEN];
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(hostTerm), hostName, NumberOf(hostName));
  heapPo h = processHeap(P);
  termPo ipList = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, &ipList);
  gcAddRoot(h, &el);

  for (int i = 0; getNthHostIP(hostName, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo) allocateCString(h, ip);
    ipList = (termPo) allocateCons(h, el, ipList);
  }

  gcReleaseRoot(h, root);
  return normalReturn((termPo)ipList);
}

/* Access host name functions */
/* return IP addresses of a host */
ReturnStatus g__hosttoip(enginePo P) {
  ValueReturn ret = s__hosttoip(P, popVal(P));

  pshVal(P, ret.value);
  return ret.status;
}

/* Access host name from IP address */
ValueReturn s__iptohost(enginePo P, termPo hostTerm) {
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(hostTerm), ip, NumberOf(ip));
  heapPo h = processHeap(P);
  char *host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(h, host);
    return normalReturn(Host);
  } else {
    return abnormalReturn(eNOTFND);
  }
}

ReturnStatus g__iptohost(enginePo P) {
  ValueReturn ret = s__iptohost(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}
