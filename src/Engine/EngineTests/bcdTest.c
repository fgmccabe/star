//
// Created by Francis McCabe on 11/12/21.
//

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "bcdTest.h"
#include "bcdP.h"

static bcdPo zeroB, oneB;

static void setupTests() {
  char *zeroNum = "0";
  char *oneNum = "1";

  zeroB = bcdFromText(zeroNum, uniStrLen(zeroNum));
  oneB = bcdFromText(oneNum, uniStrLen(oneNum));
}

static void tearDownTests() {
  free(zeroB);
  free(oneB);
}

retCode parsePositiveBCDTest() {
  char *positiveNum = "+1234567890234567891223456";
  byte data[] = {0x65, 0x43, 0x22, 0x19, 0x87, 0x65, 0x43, 0x20, 0x98, 0x76, 0x54, 0x32, 0x10};
  bcdPo check = allocBCD(positive, NumberOf(data) * 2, data);

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
  byte data[] = {0x65, 0x43, 0x22, 0x19, 0x87, 0x65, 0x43, 0x20, 0x98, 0x76, 0x54, 0x32, 0x10};
  bcdPo check = allocBCD(negative, NumberOf(data) * 2, data);

  bcdPo neg = bcdFromText(negativeNum, uniStrLen(negativeNum));

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

retCode bcdTests() {
  setupTests();
  tryRet(run_test(parsePositiveBCDTest));
  tryRet(run_test(parseNegativeBCDTest));
  tryRet(run_test(addBCDTest));
  tryRet(run_test(doubleBCDTest));
  tearDownTests();
  return Ok;
}

