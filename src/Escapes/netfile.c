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

ReturnStatus g__listen(heapPo h, termPo a1) {
  integer port = integerVal(a1);
  char nBuff[MAXFILELEN];
  ioPo listen;

  strMsg(nBuff, NumberOf(nBuff), "listen@%ld", port);
  switchProcessState(currentProcess, wait_io);
  listen = O_IO(listeningPort(nBuff, (unsigned short) port));
  setProcessRunnable(currentProcess);

  if (listen == NULL)
    return (ReturnStatus) {.ret=Abnormal, .result=eNOPERM};
  else {
    return (ReturnStatus) {.ret=Normal,
      .result =(termPo) allocateIOChnnl(h, listen)};
  }
}

ReturnStatus g__accept(heapPo h, termPo a1, termPo a2) {
  ioPo listen = ioChannel(C_IO(a1));
  ioEncoding enc = pickEncoding(integerVal(a2));

  switchProcessState(currentProcess, wait_io);

  ioPo inC, outC;

  switchProcessState(currentProcess, wait_io);
  retCode ret = acceptConnection(O_SOCK(listen), enc, &inC, &outC);

  setProcessRunnable(currentProcess);

  if (listen == NULL)
    return (ReturnStatus) {.ret=Abnormal, .result=eNOPERM};
  else {

    switch (ret) {
      case Ok: {
        int port;
        char pBuff[MAXFILELEN];
        char *peerN = peerName(O_SOCK(inC), &port);
        char *peerI = peerIP(O_SOCK(inC), &port, &pBuff[0], NumberOf(pBuff));

        if (peerN == NULL || peerI == NULL) {
          closeIo(inC);
          closeIo(outC);
          return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};
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

        return (ReturnStatus) {.ret=Normal, .result =(termPo) reslt};
      }
      default:
        return (ReturnStatus) {.ret=Abnormal, .result=eIOERROR};
    }
  }
}

ReturnStatus g__connect(heapPo h, stackPo stk) {
  integer hLen;
  const char *host = strVal(popStack(stk), &hLen);
  integer port = integerVal(popStack(stk));

  ioEncoding enc = pickEncoding(integerVal(popStack(stk)));

  switchProcessState(currentProcess, wait_io);

  ioPo inC, outC;
  retCode ret = connectRemote(host, (int) port, enc, True, &inC, &outC);

  setProcessRunnable(currentProcess);

  switch (ret) {
    case Ok: {
      heapPo H = h;
      termPo inChnl = (termPo) allocateIOChnnl(H, inC);
      int root = gcAddRoot(H, &inChnl);

      termPo outChnl = (termPo) allocateIOChnnl(H, outC);
      gcAddRoot(H, &outChnl);

      normalPo reslt = allocateTpl(H, 2);

      gcReleaseRoot(H, root);

      return (ReturnStatus) {.ret=Normal, .result =(termPo) reslt};
    }
    default:
      logMsg(logFile, "Failed to establish connection: %S", host, hLen);
      return (ReturnStatus) {.ret=Abnormal, .result=eCONNECT};
  }
}

/* Access host name functions */
/* return IP addresses of a host */
ReturnStatus g__hosttoip(heapPo h, termPo a1) {
  char host[MAXFILELEN];
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(a1), host, NumberOf(host));

  termPo ipList = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, &ipList);
  gcAddRoot(h, &el);

  for (int i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    el = (termPo) allocateCString(h, ip);
    ipList = (termPo) allocateCons(h, el, ipList);
  }

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Normal, .result =(termPo) ipList};
}

/* Access host name from IP address */
ReturnStatus g__iptohost(heapPo h, termPo a1) {
  char ip[MAX_SYMB_LEN];

  copyChars2Buff(C_STR(a1), ip, NumberOf(ip));

  char *host = getHostname(ip);

  if (host != NULL) {
    termPo Host = allocateCString(h, host);
    return (ReturnStatus) {.ret=Normal, .result =Host};
  } else
    return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};
}



