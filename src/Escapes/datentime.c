//
// Created by Francis McCabe on 3/9/18.
//

#include <arith.h>
#include <math.h>
#include <stringBuffer.h>
#include <array.h>
#include <str.h>
#include <arithP.h>
#include <tpl.h>
#include "datentime.h"

#define DATE_DOW 0
#define DATE_DAY 1
#define DATE_MON 2
#define DATE_YEAR 3
#define DATE_HOUR 4
#define DATE_MIN 5
#define DATE_SEC 6
#define DATE_UTC 7
#define DATE_ZONE 8
#define DATE_LEN 9

ReturnStatus g__date2time(processPo p, ptrPo tos) {
  normalPo dte = C_TERM(tos[0]);
  struct tm now;

  now.tm_year = (int) (integerVal(nthArg(dte, DATE_YEAR)) - 1900); /* Extract the year */
  now.tm_mon = (int) (integerVal(nthArg(dte, DATE_MON)) - 1); /* Extract the month */
  now.tm_mday = (int) (integerVal(nthArg(dte, DATE_DAY))); /* Extract the day of the month */
  now.tm_hour = (int) (integerVal(nthArg(dte, DATE_HOUR))); /* Extract the hour */
  now.tm_min = (int) (integerVal(nthArg(dte, DATE_MIN))); /* Extract the minute */

  double sec;
  double fraction = modf(floatVal(nthArg(dte, DATE_SEC)), &sec);           // Extract the second)

  now.tm_sec = (int) sec;
  now.tm_gmtoff = (int) integerVal(nthArg(dte, DATE_UTC));
  now.tm_isdst = -1;

  time_t when = mktime(&now);

  ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateFloat(processHeap(p), when + fraction)};
  return rt;
}

ReturnStatus g__utc2time(processPo p, ptrPo tos) {
  normalPo dte = C_TERM(tos[0]);
  struct tm now;

  now.tm_year = (int) (integerVal(nthArg(dte, DATE_YEAR)) - 1900); /* Extract the year */
  now.tm_mon = (int) (integerVal(nthArg(dte, DATE_MON)) - 1); /* Extract the month */
  now.tm_mday = (int) (integerVal(nthArg(dte, DATE_DAY))); /* Extract the day of the month */
  now.tm_hour = (int) (integerVal(nthArg(dte, DATE_HOUR))); /* Extract the hour */
  now.tm_min = (int) (integerVal(nthArg(dte, DATE_MIN))); /* Extract the minute */

  double sec;
  double fraction = modf(floatVal(nthArg(dte, DATE_SEC)), &sec);           // Extract the second)

  now.tm_sec = (int) sec;
  now.tm_gmtoff = (int) integerVal(nthArg(dte, DATE_UTC));
  now.tm_isdst = -1;

  time_t when = timegm(&now);

  ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateFloat(processHeap(p), when + fraction)};
  return rt;
}

ReturnStatus g__time2date(processPo p, ptrPo tos) {
  time_t when = (time_t) floatVal(tos[0]);

  struct tm *now = localtime(&when);
  heapPo H = processHeap(p);
  normalPo dte = allocateTpl(H, DATE_LEN);
  int root = gcAddRoot(H, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(tos[0]), &sec);

  intPo year = allocateInteger(H, now->tm_year + 1900);
  setArg(dte, DATE_YEAR, (termPo) year);

  intPo mon = allocateInteger(H, now->tm_mon + 1);
  setArg(dte, DATE_MON, (termPo) mon);

  intPo day = allocateInteger(H, now->tm_mday);
  setArg(dte, DATE_DAY, (termPo) day);

  intPo dow = allocateInteger(H, now->tm_wday);
  setArg(dte, DATE_DOW, (termPo) dow);

  intPo hr = allocateInteger(H, now->tm_hour);
  setArg(dte, DATE_HOUR, (termPo) hr);

  intPo min = allocateInteger(H, now->tm_min);
  setArg(dte, DATE_MIN, (termPo) min);

  fltPo sc = allocateFloat(H, now->tm_sec + fraction);
  setArg(dte, DATE_SEC, (termPo) sc);

  intPo off = allocateInteger(H, now->tm_gmtoff);
  setArg(dte, DATE_UTC, (termPo) off);

  stringPo zone = allocateCString(H, now->tm_zone);
  setArg(dte, DATE_ZONE, (termPo) zone);

  gcReleaseRoot(H, root);
  ReturnStatus rt = {.ret=Ok, .rslt=(termPo) dte};
  return rt;
}

ReturnStatus g__time2utc(processPo p, ptrPo tos) {
  time_t when = (time_t) floatVal(tos[0]);

  struct tm *now = gmtime(&when);

  heapPo H = processHeap(p);
  normalPo dte = allocateTpl(H, DATE_LEN);
  int root = gcAddRoot(H, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(tos[0]), &sec);

  intPo year = allocateInteger(H, now->tm_year + 1900);
  setArg(dte, DATE_YEAR, (termPo) year);

  intPo mon = allocateInteger(H, now->tm_mon + 1);
  setArg(dte, DATE_MON, (termPo) mon);

  intPo day = allocateInteger(H, now->tm_mday);
  setArg(dte, DATE_DAY, (termPo) day);

  intPo dow = allocateInteger(H, now->tm_wday);
  setArg(dte, DATE_DOW, (termPo) dow);

  intPo hr = allocateInteger(H, now->tm_hour);
  setArg(dte, DATE_HOUR, (termPo) hr);

  intPo min = allocateInteger(H, now->tm_min);
  setArg(dte, DATE_MIN, (termPo) min);

  fltPo sc = allocateFloat(H, now->tm_sec + fraction);
  setArg(dte, DATE_SEC, (termPo) sc);

  intPo off = allocateInteger(H, now->tm_gmtoff);
  setArg(dte, DATE_UTC, (termPo) off);

  stringPo zone = allocateCString(H, now->tm_zone);
  setArg(dte, DATE_ZONE, (termPo) zone);

  gcReleaseRoot(H, root);
  ReturnStatus rt = {.ret=Ok, .rslt=(termPo) dte};
  return rt;
}
