//
// Created by Francis McCabe on 3/9/18.
//

#include <arith.h>
#include <math.h>
#include <stringBuffer.h>
#include <strings.h>
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

ReturnStatus g__date2time(processPo p, heapPo h, termPo a1) {
  normalPo dte = C_NORMAL(a1);
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

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateFloat(h, when + fraction)};
}

ReturnStatus g__utc2time(processPo p, heapPo h, termPo a1) {
  normalPo dte = C_NORMAL(a1);
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

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateFloat(h, when + fraction)};
}

ReturnStatus g__time2date(processPo p, heapPo h, termPo a1) {
  time_t when = (time_t) floatVal(a1);

  struct tm *now = localtime(&when);
  normalPo dte = allocateTpl(h, DATE_LEN);
  int root = gcAddRoot(h, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(a1), &sec);

  termPo year = allocateInteger(h, now->tm_year + 1900);
  setArg(dte, DATE_YEAR, year);

  termPo mon = allocateInteger(h, now->tm_mon + 1);
  setArg(dte, DATE_MON, mon);

  termPo day = allocateInteger(h, now->tm_mday);
  setArg(dte, DATE_DAY, day);

  termPo dow = allocateInteger(h, now->tm_wday);
  setArg(dte, DATE_DOW, dow);

  termPo hr = allocateInteger(h, now->tm_hour);
  setArg(dte, DATE_HOUR, hr);

  termPo min = allocateInteger(h, now->tm_min);
  setArg(dte, DATE_MIN, min);

  termPo sc = allocateFloat(h, now->tm_sec + fraction);
  setArg(dte, DATE_SEC, sc);

  termPo off = allocateInteger(h, now->tm_gmtoff);
  setArg(dte, DATE_UTC, off);

  termPo zone = allocateCString(h, now->tm_zone);
  setArg(dte, DATE_ZONE, zone);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) dte};
}

ReturnStatus g__time2utc(processPo p, heapPo h, termPo a1) {
  time_t when = (time_t) floatVal(a1);

  struct tm *now = gmtime(&when);

  normalPo dte = allocateTpl(h, DATE_LEN);
  int root = gcAddRoot(h, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(a1), &sec);

  termPo year = allocateInteger(h, now->tm_year + 1900);
  setArg(dte, DATE_YEAR, year);

  termPo mon = allocateInteger(h, now->tm_mon + 1);
  setArg(dte, DATE_MON, mon);

  termPo day = allocateInteger(h, now->tm_mday);
  setArg(dte, DATE_DAY, day);

  termPo dow = allocateInteger(h, now->tm_wday);
  setArg(dte, DATE_DOW, dow);

  termPo hr = allocateInteger(h, now->tm_hour);
  setArg(dte, DATE_HOUR, hr);

  termPo min = allocateInteger(h, now->tm_min);
  setArg(dte, DATE_MIN, min);

  termPo sc = allocateFloat(h, now->tm_sec + fraction);
  setArg(dte, DATE_SEC, sc);

  termPo off = allocateInteger(h, now->tm_gmtoff);
  setArg(dte, DATE_UTC, off);

  termPo zone = allocateCString(h, now->tm_zone);
  setArg(dte, DATE_ZONE, zone);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Ok, .result= (termPo)dte};
}
