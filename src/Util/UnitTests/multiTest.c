//
// Created by Francis McCabe on 11/23/21.
//

#include <formioP.h>
#include <assert.h>
#include "multiTests.h"
#include "bcd.h"

static multiPo zeroM, oneM;

static void setupTests() {
  char *zeroNum = "0";
  char *oneNum = "1";

  zeroM = multiFromText(zeroNum, uniStrLen(zeroNum));
  oneM = multiFromText(oneNum, uniStrLen(oneNum));
  installMsgProc('M', showMulti);
}

static void tearDownTests() {
  freeMulti(zeroM);
  freeMulti(oneM);
}

retCode lg2Tests(){
  integer cx = 1;
  for(integer i=0;i<63;i++){
    assert(lg2(cx<<i)==i);
  }
  return Ok;
}

retCode parsePositiveMultiTest() {
  char *positiveNum = "18446744073709551612";
  uint32 data[] = {0xfffffffc, 0xffffffff, 0x0};
  multiPo check = allocMulti(data, NumberOf(data));

  // Check number parses to little endian format
  multiPo pos = multiFromText(positiveNum, uniStrLen(positiveNum));

  assert(multiCompare(check, pos) == same);

  char rtnVal[64];
  strMsg(rtnVal, NumberOf(rtnVal), "%M", pos);

  assert(uniNCmp(positiveNum, uniStrLen(positiveNum), rtnVal, uniStrLen(rtnVal)) == same);

  freeMulti(pos);
  freeMulti(check);
  return Ok;
}

retCode parseNegativeMultiTest() {
  char *negativeNum = "-18446744073709551612";
  uint32 data[] = {0x4, 0x0, 0xffffffff};
  multiPo check = allocMulti(data, NumberOf(data));

  // Check number parses to little endian format
  multiPo neg = multiFromText(negativeNum, uniStrLen(negativeNum));

  assert(multiCompare(check, neg) == same);

  char txt[64];
  strMsg(txt, NumberOf(txt), "%M", neg);

  assert(uniNCmp(negativeNum, uniStrLen(negativeNum), txt, uniStrLen(negativeNum)) == same);

  freeMulti(neg);
  freeMulti(check);
  return Ok;
}

retCode formattedNegativeMultiTest() {
  char *negativeNum = "-18446744073709551612";

  multiPo neg = multiFromText(negativeNum, uniStrLen(negativeNum));

  char txt[128];
  char *format = "(99999999999.9999999)";
  integer len = formatMulti(neg, format, uniStrLen(format), txt, 128);

  assert(uniNCmp(txt, len, "(18446.7440737)", uniStrLen("(18446.7440737)")) == same);

  freeMulti(neg);

  return Ok;
}

retCode checkDivBy10() {
  char *positiveNum = "1234567890";
  multiPo pos = multiFromText(positiveNum, uniStrLen(positiveNum));
  uint32 data[] = {0x499602D2};
  multiPo check = allocMulti(data, NumberOf(data));

  assert(sameMulti(pos, check));

  char temp[30];
  strMsg(temp, NumberOf(temp), "%M", pos);

  assert(uniNCmp(temp, uniStrLen(temp), positiveNum, uniStrLen(positiveNum)) == same);
  return Ok;
}

retCode addMultiTest() {
  char *astr = "1234567890234567891223456";
  multiPo a = multiFromText(astr, uniStrLen(astr));

  char *bstr = "-1234567890234567891223456";
  multiPo b = multiFromText(bstr, uniStrLen(bstr));

  multiPo sum = multiPlus(a, b);

  assert(sameMulti(zeroM, sum));

  multiPo c = multiPlus(b, oneM);
  multiPo s2 = multiPlus(a, c);
  assert(sameMulti(oneM, s2));

  freeMulti(a);
  freeMulti(b);
  freeMulti(sum);
  freeMulti(s2);
  return Ok;
}

retCode minusMultiTest() {
  char *astr = "1234567890234567891223456";
  multiPo a = multiFromText(astr, uniStrLen(astr));

  char *bstr = "-1234567890234567891223456";
  multiPo b = multiFromText(bstr, uniStrLen(bstr));

  multiPo sum = multiMinus(a, a);

  assert(sameMulti(zeroM, sum));

  freeMulti(a);
  freeMulti(b);
  freeMulti(sum);
  return Ok;
}

retCode multiMulTest() {
  char *aStr = "23958233";
  multiPo bigger = multiFromText(aStr, uniStrLen(aStr));

  char *bStr = "5830";
  multiPo smaller = multiFromText(bStr, uniStrLen(bStr));

  char *checkNum = "139676498390";
  multiPo check = multiFromText(checkNum, uniStrLen(checkNum));

  multiPo res = multiTimes(bigger, smaller);
  outMsg(logFile, "%M*%M is %M\n%_", bigger, smaller, res);

  assert(sameMulti(res, check));
  return Ok;
}

retCode factorialTest() {
  multiPo ix = oneM;
  multiPo limit = multiFromText("101", 3);
  multiPo prod = oneM;

  char *fact100 = "9332621544394415268169923885626670049071596826438162146859"
                  "29638952175999932299156089414639761565182862536979208272237"
                  "58251185210916864000000000000000000000000";
  multiPo check = multiFromText(fact100, uniStrLen(fact100));

  while (!sameMulti(ix, limit)) {
    prod = multiTimes(prod, ix);
//    outMsg(logFile, "%M! is %M\n%_", ix, prod);

    ix = multiPlus(ix, oneM);
  }
  outMsg(logFile, "100! is %M\n%_", prod);
  assert(sameMulti(prod, check));

  return Ok;
}

retCode ePiTest() {
  multiPo e = multiFromStr(
    "27182818284590452353602874713526624977572470936999595749669676277240766303535945713821785251664274");
  multiPo pi = multiFromStr(
    "31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679");

  multiPo prod = multiTimes(e, pi);

  multiPo check = multiFromStr("853973422267356706546355086954657449503488853576511496187960113017922"
                               "861115747838957625075000327286593478162070678673043735453603785848133"
                               "094851462428844661315312664336578255202869537848950160622046");

  assert(sameMulti(prod, check));

  return Ok;
}

retCode multiDivideTest() {
  char *aStr = "999999999";
  multiPo a = multiFromText(aStr, uniStrLen(aStr));

  char *bStr = "999999";
  multiPo b = multiFromText(bStr, uniStrLen(bStr));

  multiPo checkQuot = multiFromStr("1000");
  multiPo checkRem = multiFromStr("999");

  multiPo quot, rem;
  multiDivide(&quot, &rem, a, b);
  outMsg(logFile, "%M/%M is %M + %M\n%_", a, b, quot, rem);

  assert(sameMulti(quot, checkQuot));
  assert(sameMulti(rem, checkRem));
  return Ok;
}

retCode multiDivide2Test() {
  // 40!/15!
  multiPo a = multiFromStr("815915283247897734345611269596115894272000000000");
  multiPo b = multiFromStr("1307674368000");

  multiPo checkQuot = multiFromStr("623943776229081622823099695104000000");
  multiPo checkRem = multiFromStr("0");

  multiPo quot, rem;
  multiDivide(&quot, &rem, a, b);
  outMsg(logFile, "%M/%M is %M + %M\n%_", a, b, quot, rem);

  assert(sameMulti(quot, checkQuot));
  assert(sameMulti(rem, checkRem));
  return Ok;
}


retCode multiDivideNNgTest() {
  // 40!/15!
  multiPo a = multiFromStr("-815915283247897734345611269596115894272000000000");
  multiPo b = multiFromStr("-1307674368000");

  multiPo checkQuot = multiFromStr("623943776229081622823099695104000000");
  multiPo checkRem = multiFromStr("0");

  multiPo quot, rem;
  multiDivide(&quot, &rem, a, b);
  outMsg(logFile, "%M/%M is %M + %M\n%_", a, b, quot, rem);

  assert(sameMulti(quot, checkQuot));
  assert(sameMulti(rem, checkRem));
  return Ok;
}

retCode multiTests() {
  setupTests();
  tryRet(run_test(lg2Tests));
  tryRet(run_test(checkDivBy10));
  tryRet(run_test(parsePositiveMultiTest));
  tryRet(run_test(parseNegativeMultiTest));
  tryRet(run_test(formattedNegativeMultiTest));
  tryRet(run_test(addMultiTest));
  tryRet(run_test(minusMultiTest));
  tryRet(run_test(multiMulTest));
  tryRet(run_test(factorialTest));
  tryRet(run_test(ePiTest));
  tryRet(run_test(multiDivideTest));
  tryRet(run_test(multiDivide2Test));
  tryRet(run_test(multiDivideNNgTest));
  tearDownTests();
  return Ok;
}
