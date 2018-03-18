//
// Created by Francis McCabe on 3/8/18.
//

#include <arith.h>
#include <ioTcp.h>
#include <errorCodes.h>
#include <iochnnlP.h>
#include <str.h>
#include <arithP.h>
#include <tpl.h>
#include <array.h>
#include <hosts.h>
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
    ReturnStatus ret = {.ret=Ok, .rslt =(termPo) allocateIOChnnl(processHeap(P), listen)};
    return ret;
  }
}

ReturnStatus g__accept(processPo P, ptrPo tos) {
  ioPo listen = ioChannel(C_IO(tos[1]));
  ioEncoding enc = pickEncoding(integerVal(tos[0]));

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

        termPo peer = (termPo) allocateString(H, peerN, uniStrLen(peerN));
        gcAddRoot(H, &peer);

        termPo peerIP = (termPo) allocateString(H, peerI, uniStrLen(peerI));
        gcAddRoot(H, &peerIP);

        termPo prt = (termPo) allocateInteger(H, port);
        gcAddRoot(H, &prt);

        normalPo reslt = allocateTpl(H, 5);

        setArg(reslt, 0, inChnl);
        setArg(reslt, 1, outChnl);
        setArg(reslt, 2, peer);
        setArg(reslt, 3, prt);
        setArg(reslt, 4, peerIP);

        gcReleaseRoot(H, 0);

        ReturnStatus rt = {.ret=Ok, .rslt =(termPo) reslt};

        return rt;
      }
      default:
        return liberror(P, "_accept", eIOERROR);
    }
  }
}

ReturnStatus g__connect(processPo P, ptrPo tos) {
  integer port = integerVal(tos[1]);

  integer fnLen;
  const char *host = stringVal(tos[2], &fnLen);

  ioEncoding enc = pickEncoding(integerVal(tos[0]));

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

      gcReleaseRoot(H, 0);

      ReturnStatus rt = {.ret=Ok, .rslt =(termPo) reslt};

      return rt;
    }
    default:
      logMsg(logFile, "Failed to establish connection: %U", host);
      return liberror(P, "_connect", eCONNECT);
  }
}

/* Access host name functions */
/* return IP addresses of a host */
ReturnStatus g__hosttoip(processPo P, ptrPo tos) {
  char host[MAXFILELEN];
  char ip[MAX_SYMB_LEN];
  heapPo H = processHeap(P);

  copyString2Buff(C_STR(tos[1]), host, NumberOf(host));

  listPo ipList = createList(H, 8);
  int root = gcAddRoot(H, (ptrPo) &ipList);

  for (int i = 0; getNthHostIP(host, (unsigned) i, ip, NumberOf(ip)) != NULL; i++) {
    stringPo el = allocateCString(H, ip);

    ipList = addToList(H, ipList, (termPo) el);
  }

  gcReleaseRoot(H, root);
  ReturnStatus rt = {.ret=Ok, .rslt =(termPo) ipList};

  return rt;
}

/* Access host name from IP address */
ReturnStatus g__iptohost(processPo P, ptrPo tos) {
  char ip[MAX_SYMB_LEN];
  heapPo H = processHeap(P);

  copyString2Buff(C_STR(tos[0]), ip, NumberOf(ip));

  char *host = getHostname(ip);

  if (host != NULL) {
    stringPo Host = allocateCString(H, host);
    ReturnStatus rt = {.ret=Ok, .rslt =(termPo) Host};
    return rt;
  } else
    return liberror(P, "_iptohost", eNOTFND);
}



