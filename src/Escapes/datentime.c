//
// Created by Francis McCabe on 3/9/18.
//

#include <arith.h>
#include <math.h>
#include <strings.h>
#include <arithP.h>
#include <tpl.h>
#include "globals.h"

#define DATE_DOW 0
#define DATE_DAY 1
#define DATE_MON 2
#define DATE_YEAR 3
#define DATE_HOUR 4
#define DATE_MIN 5
#define DATE_SEC 6
#define DATE_UTC 7
#define DATE_LEN 8

ReturnStatus
g__date2time(heapPo h, termPo yr, termPo mon, termPo day, termPo hour, termPo min, termPo s, termPo gmtoff) {
  struct tm now;

  now.tm_year = (int) (integerVal(yr) - 1900); /* Extract the year */
  now.tm_mon = (int) (integerVal(mon) - 1); /* Extract the month */
  now.tm_mday = (int) (integerVal(day)); /* Extract the day of the month */
  now.tm_hour = (int) (integerVal(hour)); /* Extract the hour */
  now.tm_min = (int) (integerVal(min)); /* Extract the minute */

  double sec = floatVal(s);
  double fraction = modf(sec, &sec);           // Extract the seconds

  now.tm_sec = (int) sec;
  now.tm_gmtoff = (int) integerVal(gmtoff);
  now.tm_isdst = -1;

  time_t when = mktime(&now);

  return (ReturnStatus) {.ret=Ok,
    .result=makeFloat((double) when + fraction)};
}

ReturnStatus g__utc2time(heapPo h, termPo a1) {
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
    .result=makeFloat((double) when + fraction)};
}

ReturnStatus g__time2date(heapPo h, termPo a1) {
  time_t when = (time_t) floatVal(a1);

  struct tm *now = localtime(&when);
  normalPo dte = allocateTpl(h, DATE_LEN);
  int root = gcAddRoot(h, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(a1), &sec);

  termPo year = makeInteger(now->tm_year + 1900);
  setArg(dte, DATE_YEAR, year);

  termPo mon = makeInteger(now->tm_mon + 1);
  setArg(dte, DATE_MON, mon);

  termPo day = makeInteger(now->tm_mday);
  setArg(dte, DATE_DAY, day);

  termPo dow = makeInteger(now->tm_wday);
  setArg(dte, DATE_DOW, dow);

  termPo hr = makeInteger(now->tm_hour);
  setArg(dte, DATE_HOUR, hr);

  termPo min = makeInteger(now->tm_min);
  setArg(dte, DATE_MIN, min);

  termPo sc = makeFloat(now->tm_sec + fraction);
  setArg(dte, DATE_SEC, sc);

  termPo off = makeInteger(now->tm_gmtoff);
  setArg(dte, DATE_UTC, off);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) dte};
}

ReturnStatus g__time2utc(heapPo h, termPo a1) {
  time_t when = (time_t) floatVal(a1);

  struct tm *now = gmtime(&when);

  normalPo dte = allocateTpl(h, DATE_LEN);
  int root = gcAddRoot(h, (ptrPo) &dte);

  double sec;
  double fraction = modf(floatVal(a1), &sec);

  termPo year = makeInteger(now->tm_year + 1900);
  setArg(dte, DATE_YEAR, year);

  termPo mon = makeInteger(now->tm_mon + 1);
  setArg(dte, DATE_MON, mon);

  termPo day = makeInteger(now->tm_mday);
  setArg(dte, DATE_DAY, day);

  termPo dow = makeInteger(now->tm_wday);
  setArg(dte, DATE_DOW, dow);

  termPo hr = makeInteger(now->tm_hour);
  setArg(dte, DATE_HOUR, hr);

  termPo min = makeInteger(now->tm_min);
  setArg(dte, DATE_MIN, min);

  termPo sc = makeFloat(now->tm_sec + fraction);
  setArg(dte, DATE_SEC, sc);

  termPo off = makeInteger(now->tm_gmtoff);
  setArg(dte, DATE_UTC, off);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Ok, .result= (termPo) dte};
}

ReturnStatus g__fmttime(heapPo h, termPo a1, termPo a2) {
  time_t when = (time_t) floatVal(a1);
  char fmt[256];
  if (copyChars2Buff(C_STR(a2), fmt, NumberOf(fmt)) == Ok) {
    struct tm *now = localtime(&when);

    char stamp[256];
    strftime(stamp, 256, fmt, now);

    return (ReturnStatus) {.ret=Ok, .result=allocateCString(h, stamp)};
  } else {
    return (ReturnStatus) {.ret=Error, .result=voidEnum};
  }
}

static retCode formatDate(ioPo out, const char *fmt, integer fmtLen, struct tm *time);

ReturnStatus g__formattime(heapPo h, termPo a1, termPo a2) {
  time_t when = (time_t) floatVal(a1);
  integer fmtLen;
  const char *fmt = strVal(a2, &fmtLen);

  struct tm *now = localtime(&when);

  strBufferPo buff = newStringBuffer();

  retCode ret = formatDate(O_IO(buff), fmt, fmtLen, now);

  if (ret == Ok) {
    termPo result = allocateFromStrBuffer(buff, h);
    closeFile(O_IO(buff));

    return (ReturnStatus) {.ret=Ok, .result=result};
  } else {
    closeFile(O_IO(buff));
    return (ReturnStatus) {.ret=Error, .result=voidEnum};
  }
}

static integer countFmtChrs(const char *fmt, integer *pos, integer len, codePoint f) {
  integer cx = 1;
  integer ps = *pos;

  while (ps < len) {
    integer pps = ps;
    codePoint p = nextCodePoint(fmt, &ps, len);
    if (p == f) {
      cx++;
    } else {
      *pos = pps;
      return cx;
    }
  }
  *pos = ps;
  return cx;
}

static char *shortMonths[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
static char *longMonths[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September",
                             "October", "November", "December"};

static char *shortDays[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
static char *longDays[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};

retCode formatDate(ioPo out, const char *fmt, integer fmtLen, struct tm *time) {
  integer outPos = 0;
  retCode ret = Ok;

  integer fmtPos = 0;

  while (ret == Ok && fmtPos < fmtLen) {
    codePoint f = nextCodePoint(fmt, &fmtPos, fmtLen);

    integer fLen = countFmtChrs(fmt, &fmtPos, fmtLen, f);

    switch (f) {
      case 'G': {
        logical isAD = time->tm_year >= -1900;

        ret = outMsg(out, "% .*s", clamp(1, fLen, 3), (isAD ? "CE" : "BCE"));
        continue;
      }
      case 'y': {
        int year = time->tm_year + 1900;

        fLen = clamp(2, fLen, 4);

        if (fLen == 2)
          ret = outMsg(out, "%2d", year % 100);
        else
          ret = outMsg(out, "%0:*d", fLen, year % 10000);

        continue;
      }
      case 'm': {
        int mon = time->tm_mon;

        switch (fLen) {
          case 1:
          case 2: {
            ret = outMsg(out, "%0.*d", fLen, mon + 1);
            continue;
          }
          case 3: {
            ret = outMsg(out, "%s", shortMonths[mon]);
            continue;
          }
          default:
            ret = outMsg(out, "% .*s", fLen, longMonths[mon]);
            continue;
        }
      }

      case 'w': {
        int day = time->tm_wday;

        switch (fLen) {
          case 1: {
            ret = outMsg(out, "%0:*d", fLen, day);
            continue;
          }
          case 2:
          case 3: {
            ret = outMsg(out, "% :*s", fLen, shortDays[day]);
            continue;
          }
          default:
            ret = outMsg(out, "% :*s", fLen, longDays[day]);
            continue;
        }
      }
      case 'D': {
        int day = time->tm_yday;
        ret = outMsg(out, "%0:*d", fLen, day+1);
        continue;
      }
      case 'd': {
        int day = time->tm_mday;
        ret = outMsg(out, "%0:*d", fLen, day);
        continue;
      }

      case 'a': {
        logical pm = time->tm_hour >= 12;
        ret = outMsg(out, "% :*s", fLen, (pm ? "pm" : "am"));
        continue;
      }
      case 'A': {
        logical pm = time->tm_hour >= 12;
        ret = outMsg(out, "% :*s", fLen, (pm ? "PM" : "AM"));
        continue;
      }
      case 'h': {
        int hr = time->tm_hour % 12;
        ret = outMsg(out, "%0:*d", fLen, hr);
        continue;
      }
      case 'H': {
        int hr = time->tm_hour;
        ret = outMsg(out, "%0:*d", fLen, hr);
        continue;
      }
      case 'M': {
        int min = time->tm_min;
        ret = outMsg(out, "%0:*d", fLen, min);
        continue;
      }
      case 'S': {
        int sec = time->tm_sec;
        ret = outMsg(out, "%0:*d", fLen, sec);
        continue;
      }
      case 'z': {
        ret = outMsg(out, "% :*s", fLen, time->tm_zone);
        continue;
      }
      default: {
        for (int ix = 0; ret == Ok && ix < fLen; ix++) {
          ret = outChar(out, f);
        }
        continue;
      }
    }
  }
  return ret;
}

