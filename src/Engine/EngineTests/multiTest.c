//
// Created by Francis McCabe on 11/23/21.
//

#include <formioP.h>
#include <stdlib.h>
#include <assert.h>
#include "multiTests.h"

static multiPo zeroB, oneB;

static void setupTests() {
  char *zeroNum = "0";
  char *oneNum = "1";

  zeroB = multiFromText(zeroNum, uniStrLen(zeroNum));
  oneB = multiFromText(oneNum, uniStrLen(oneNum));
//  installMsgProc('M', showMulti);
}

static void tearDownTests() {
  free(zeroB);
  free(oneB);
}

retCode parsePositiveMultiTest() {
  char *positiveNum = "+18446744073709551612";
  uint32 data[] =  {0xfffffffc, 0xffffffff, 0x0 };
  multiPo check = allocMulti(data,NumberOf(data));

  // Check number parses to little endian format
  multiPo pos = multiFromText(positiveNum, uniStrLen(positiveNum));

  assert(multiCompare(check, pos)==same);

  char rtnVal[64];
  integer tlen = multiText(rtnVal, NumberOf(rtnVal), pos);

  assert(uniNCmp(positiveNum, uniStrLen(positiveNum), rtnVal, tlen) == same);

  free(pos);
  free(check);
  return Ok;
}

//retCode parseNegativeMultiTest() {
//  char *positiveNum = "-1234567890234567891223456";
//  uint32 data[] = {0x91223456, 0x02345678, 0x23456789, 0x1};
//  multiPo check = allocMulti(data,NumberOf(data));
//
//  // Check number parses to little endian format
//  multiPo pos = multiFromText(positiveNum, uniStrLen(positiveNum));
//
//  assert(multiCompare(check, pos)==same);
////
////  char rtnVal[64];
////  integer tlen = textOfmulti(rtnVal, NumberOf(rtnVal), pos);
////
////  assert(uniNCmp(positiveNum, uniStrLen(positiveNum), rtnVal, tlen) == same);
//
//  free(pos);
//  free(check);
//  return Ok;
//}

retCode checkDivBy10() {
  char *positiveNum = "+1234567890";
  multiPo pos = multiFromText(positiveNum, uniStrLen(positiveNum));
  uint32 data[] = {0x499602D2};
  multiPo check = allocMulti(data, NumberOf(data));

  assert(multiCompare(pos, check) == same);

  char temp[30];
  integer len = multiText(temp, 30, pos);

  assert(uniNCmp(temp, len, positiveNum, uniStrLen(positiveNum))==same);
  return Ok;
}

retCode multiTests() {
  setupTests();
  tryRet(run_test(checkDivBy10));
  tryRet(run_test(parsePositiveMultiTest));
//  tryRet(run_test(parseNegativeMultiTest));
  tearDownTests();
  return Ok;
}
