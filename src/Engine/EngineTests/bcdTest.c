//
// Created by Francis McCabe on 11/12/21.
//

#include <assert.h>
#include <stdlib.h>
#include "formioP.h"
#include "bcdTest.h"
#include "bcdP.h"

static bcdPo zeroB, oneB;

static void setupTests() {
  char *zeroNum = "0";
  char *oneNum = "1";

  zeroB = bcdFromText(zeroNum, uniStrLen(zeroNum));
  oneB = bcdFromText(oneNum, uniStrLen(oneNum));
  installMsgProc('B', showBCD);
}

static void tearDownTests() {
  free(zeroB);
  free(oneB);
}

retCode testB32() {
  uint32 a = 0x12345678;
  uint32 b = 0x12345678;
  uint64 c = bcd_add(a, b);
  assert(c == 0x24691356);
  return Ok;
}

retCode testO32() {
  uint32 a = 0x49345678;
  uint32 b = 0x49345678;
  uint64 c = bcd_add(a, b);
  assert(c == 0x98691356);
  uint64 d = bcd_add(0x49345678, c);
  assert(d == 0x148037034);

  return Ok;
}

retCode test1000() {
  uint32 a = 0x1;
  uint64 t = 0;
  for (integer ix = 0; ix < 10000; ix++)
    t = bcd_add(t, a);
  assert(t == 0x10000);

  return Ok;
}

retCode parsePositiveBCDTest() {
  char *positiveNum = "+1234567890234567891223456";
  uint32 data[] = {0x91223456, 0x02345678, 0x23456789, 0x1};
  bcdPo check = allocBCD(NumberOf(data), data);

  // Check number parses to little endian format
  bcdPo pos = bcdFromText(positiveNum, uniStrLen(positiveNum));

  assert(sameBCD(check, pos));

  char rtnVal[64];
  integer tlen = textOfBCD(rtnVal, NumberOf(rtnVal), pos);

  assert(uniNCmp(positiveNum, uniStrLen(positiveNum), rtnVal, tlen) == same);

  free(pos);
  free(check);
  return Ok;
}

retCode parseNegativeBCDTest() {
  char *negativeNum = "-1234567890234567891223456";
  uint32 data[] = {0x08776544, 0x97654321, 0x76543210, 0x99999998};
  bcdPo check = allocBCD(NumberOf(data), data);

  bcdPo neg = bcdFromText(negativeNum, uniStrLen(negativeNum));

//  outMsg(logFile,"Parsed %B\n%_",neg);

  assert(sameBCD(check, neg));

  char rtnVal[64];
  integer tlen = textOfBCD(rtnVal, NumberOf(rtnVal), neg);

  assert(uniNCmp(negativeNum, uniStrLen(negativeNum), rtnVal, tlen) == same);
  free(neg);
  free(check);
  return Ok;
}

retCode addBCDTest() {
  char *positiveNum = "1234567890234567891223456";
  bcdPo pos = bcdFromText(positiveNum, uniStrLen(positiveNum));

  char *negativeNum = "-1234567890234567891223456";

  bcdPo neg = bcdFromText(negativeNum, uniStrLen(negativeNum));

  bcdPo sum = bcdPlus(pos, neg);

  assert(sameBCD(zeroB, sum));

  free(pos);
  free(neg);
  free(sum);
  return Ok;
}

retCode doubleBCDTest() {
  char *positiveNum = "1234567890234567891223456";
  bcdPo pos = bcdFromText(positiveNum, uniStrLen(positiveNum));

  bcdPo dbl = bcdPlus(pos, pos);

  bcdPo ori = bcdMinus(dbl, pos);

  assert(sameBCD(ori, pos));

  free(pos);
  free(dbl);
  free(ori);
  return Ok;
}

retCode additionBCDTest() {
  char *positiveNum = "1234567890";
  bcdPo pos = bcdFromText(positiveNum, uniStrLen(positiveNum));

  char *zed = "0";
  bcdPo posAcc = bcdFromText(zed, uniStrLen(zed));

  integer runCount = 100;

  for (integer ix = 0; ix < runCount; ix++) {
    bcdPo intm = bcdPlus(posAcc, pos);
    free(posAcc);
    posAcc = intm;
//    outMsg(logFile,"pos = %B\n%_",posAcc);
  }

  bcdPo negAcc = bcdFromText(zed, uniStrLen(zed));

  for (integer ix = 0; ix < runCount; ix++) {
    bcdPo intn = bcdMinus(negAcc, pos);
//    outMsg(logFile, "%B - %B = %B\n%_", negAcc, pos, intn);
    free(negAcc);
    negAcc = intn;
  }

//  outMsg(logFile, "pos = %B\nneg = %B\n", posAcc, negAcc);
  bcdPo res = bcdPlus(posAcc, negAcc);

  assert(sameBCD(res, zeroB));

  free(pos);
  free(res);
  free(posAcc);
  free(negAcc);
  return Ok;
}

retCode bcdSqTest() {
  char *positiveNum = "1234567890";
  bcdPo num = bcdFromText(positiveNum, uniStrLen(positiveNum));

  char *negativeNum = "-1234567890";
  bcdPo neg = bcdFromText(negativeNum, uniStrLen(negativeNum));

  char *posCheckTxt = "1524157875019052100";
  bcdPo posCheck = bcdFromText(posCheckTxt, uniStrLen(posCheckTxt));

  char *negCheckTxt = "-1524157875019052100";
  bcdPo negCheck = bcdFromText(negCheckTxt, uniStrLen(negCheckTxt));

  bcdPo res = bcdTimes(num, num);
//  outMsg(logFile, "Square of %B is %B\n", num, res);
  assert(sameBCD(res, posCheck));

  bcdPo nres = bcdTimes(neg, num);
  outMsg(logFile, "%B*%B is %B\n", neg, num, nres);

  assert(sameBCD(nres, negCheck));

  bcdPo nres2 = bcdTimes(num, neg);
  outMsg(logFile, "%B*%B is %B\n", num, neg, nres2);

  assert(sameBCD(nres2, negCheck));

  bcdPo sqres = bcdTimes(neg, neg);
  outMsg(logFile, "Square of %B is %B\n%_", neg, sqres);
  assert(sameBCD(sqres, posCheck));

  return Ok;
}

retCode bcdMulTest() {
  char *biggerText = "23958233";
  bcdPo bigger = bcdFromText(biggerText, uniStrLen(biggerText));
  char *smallerText = "5830";
  bcdPo smaller = bcdFromText(smallerText, uniStrLen(smallerText));

  char *checkNum = "139676498390";
  bcdPo check = bcdFromText(checkNum, uniStrLen(checkNum));

  bcdPo res = bcdTimes(bigger, smaller);
  outMsg(logFile, "%B*%B is %B\n", bigger, smaller, res);

  assert(sameBCD(res, check));
  return Ok;
}

retCode bcdTests() {
  setupTests();
  tryRet(run_test(testB32));
  tryRet(run_test(testO32));
  tryRet(run_test(test1000));
  tryRet(run_test(parsePositiveBCDTest));
  tryRet(run_test(parseNegativeBCDTest));
  tryRet(run_test(addBCDTest));
  tryRet(run_test(doubleBCDTest));
  tryRet(run_test(additionBCDTest));
  tryRet(run_test(bcdSqTest));
  tryRet(run_test(bcdMulTest));
  tearDownTests();
  return Ok;
}

