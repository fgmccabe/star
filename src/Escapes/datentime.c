//
// Created by Francis McCabe on 3/9/18.
//

#include <arith.h>
#include <math.h>
#include <strings.h>
#include <arithP.h>
#include <tpl.h>
#include "globals.h"
#include "clock.h"
#include "option.h"
#include "errorCodes.h"

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

  return (ReturnStatus) {.ret=Normal,
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

  return (ReturnStatus) {.ret=Normal,
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

  termPo tmOffset = makeInteger(now->tm_gmtoff);
  setArg(dte, DATE_UTC, tmOffset);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Normal, .result=(termPo) dte};
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

  termPo tmOffset = makeInteger(now->tm_gmtoff);
  setArg(dte, DATE_UTC, tmOffset);

  gcReleaseRoot(h, root);
  return (ReturnStatus) {.ret=Normal, .result= (termPo) dte};
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
    termPo result = allocateFromStrBuffer(h, buff);
    closeIo(O_IO(buff));

    return (ReturnStatus) {.ret=Normal, .result=result};
  } else {
    closeIo(O_IO(buff));
    return (ReturnStatus) {.ret=Abnormal, .cont = Null, .result=eINVAL};
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
  retCode ret = Ok;
  integer fmtPos = 0;

  while (ret == Ok && fmtPos < fmtLen) {
    codePoint f = nextCodePoint(fmt, &fmtPos, fmtLen);

    integer fLen = countFmtChrs(fmt, &fmtPos, fmtLen, f);

    switch (f) {
      case 'G': {
        logical isAD = time->tm_year >= -1900;

        ret = outMsg(out, "% :*s", clamp(1, fLen, 3), (isAD ? "CE" : "BCE"));
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
            ret = outMsg(out, "%0:*d", fLen, mon + 1);
            continue;
          }
          case 3: {
            ret = outMsg(out, "%s", shortMonths[mon]);
            continue;
          }
          default:
            ret = outMsg(out, "% :*s", fLen, longMonths[mon]);
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
        ret = outMsg(out, "%0:*d", fLen, day + 1);
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
      case 'Z': {
        integer gmtoffset = (integer) time->tm_gmtoff;
        int hours = ((int) absolute(gmtoffset)) / (60 * 60);
        int mins = (((int) absolute(gmtoffset)) / 60) % 60;
        char zone[32];

        switch (fLen) {
          case 3:
            strMsg(zone, NumberOf(zone), "%s%0:2d", (gmtoffset < 0 ? "-" : "+"), hours);
            break;
          case 5:
            strMsg(zone, NumberOf(zone), "%s%0:2d%0:2d", (gmtoffset < 0 ? "-" : "+"), hours, mins);
            break;
          default:
            strMsg(zone, NumberOf(zone), "%s%0:2d:%0:2d", (gmtoffset < 0 ? "-" : "+"), hours, mins);
            break;
        }

        ret = outMsg(out, "%+ :*s", fLen, zone);
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

static retCode rdNum(const char *src, integer *srcPos, integer srcLen, int count, int *res) {
  int x = 0;
  retCode ret = Ok;
  for (int ix = 0; ix < count && ret == Ok && *srcPos < srcLen; ix++) {
    codePoint ch = nextCodePoint(src, srcPos, srcLen);
    if (isNdChar(ch)) {
      x = x * 10 + digitValue(ch);
    } else
      ret = Fail;
  }
  *res = x;
  return ret;
}

static retCode
rdChars(const char *src, integer *srcPos, integer srcLen, int count, char *res, integer *resPos, integer resLen) {
  retCode ret = Ok;
  for (int ix = 0; ix < count && ret == Ok && *srcPos < srcLen; ix++) {
    codePoint ch = nextCodePoint(src, srcPos, srcLen);
    ret = appendCodePoint(res, resPos, resLen, ch);
  }
  if (*resPos < resLen)
    res[*resPos] = '\0';
  return ret;
}

static retCode parseTime(const char *fmt, integer fmtLen, const char *src, integer srcLen, struct tm *time) {
  retCode ret = Ok;

  integer fmtPos = 0;
  integer srcPos = 0;

  logical isCE = True;
  logical isPm = False;
  int year = 0;
  int month = 0;
  int day = 0;
  int hours = 0;
  int mins = 0;
  int secs = 0;
  long gmtOffset = timezone_offset;

  while (ret == Ok && fmtPos < fmtLen && srcPos < srcLen) {
    codePoint f = nextCodePoint(fmt, &fmtPos, fmtLen);
    integer fLen = countFmtChrs(fmt, &fmtPos, fmtLen, f);

    switch (f) {
      case 'G': {
        char adBuff[16];
        integer adPos = 0;
        ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, 3), adBuff, &adPos, NumberOf(adBuff));

        isCE = uniIsLit(adBuff, "CE") || uniIsLit(adBuff, "ce") || uniIsLit(adBuff, "AD") || uniIsLit(adBuff, "ad");
        continue;
      }
      case 'y': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(2, fLen, 4), &year);
        continue;
      }
      case 'm': {
        char mnBuff[16];
        integer mnPos = 0;

        switch (fLen) {
          case 1:
          case 2: {
            ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 2), &month);
            month--; // Map January to 0
            continue;
          }
          case 3: {
            ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, 3), mnBuff, &mnPos, NumberOf(mnBuff));

            for (int ix = 0; ret == Ok && ix < NumberOf(shortMonths); ix++) {
              if (uniIsLit(shortMonths[ix], mnBuff)) {
                time->tm_mon = ix;
                break;
              }
            }
            continue;
          }
          default: {
            ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, NumberOf(mnBuff)), mnBuff, &mnPos,
                          NumberOf(mnBuff));

            for (int ix = 0; ret == Ok && ix < NumberOf(longMonths); ix++) {
              if (uniIsLit(longMonths[ix], mnBuff)) {
                time->tm_mon = ix;
                break;
              }
            }
            continue;
          }
        }
      }
      case 'w': {
        char wkBuff[16];
        integer mnPos = 0;
        switch (fLen) {
          case 1:
          case 2: {
            ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 2), &time->tm_wday);
            continue;
          }
          case 3: {
            ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, 3), wkBuff, &mnPos, NumberOf(wkBuff));

            for (int ix = 0; ret == Ok && ix < NumberOf(shortDays); ix++) {
              if (uniIsLit(shortDays[ix], wkBuff)) {
                time->tm_wday = ix;
                break;
              }
            }
            continue;
          }
          default: {
            ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, NumberOf(wkBuff)), wkBuff, &mnPos,
                          NumberOf(wkBuff));

            for (int ix = 0; ret == Ok && ix < NumberOf(longDays); ix++) {
              if (uniIsLit(longDays[ix], wkBuff)) {
                time->tm_wday = ix;
                break;
              }
            }
            continue;
          }
        }
      }
      case 'D': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 4), &time->tm_yday);
        continue;
      }
      case 'd': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 4), &day);
        continue;
      }
      case 'a':
      case 'A': {
        char ampmBuff[16];
        integer adPos = 0;
        ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, 3), ampmBuff, &adPos, NumberOf(ampmBuff));

        isPm = uniIsLit(ampmBuff, "pm") || uniIsLit(ampmBuff, "PM");
        continue;
      }
      case 'H':
      case 'h': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 2), &hours);
        continue;
      }

      case 'M': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 2), &mins);
        continue;
      }
      case 'S': {
        ret = rdNum(src, &srcPos, srcLen, (int) clamp(1, fLen, 2), &time->tm_sec);
        continue;
      }
      case 'z': {
        ret = Error;
        continue;
      }
      case 'Z': {
        char znBuff[16];
        integer znLen = 0;
        ret = rdChars(src, &srcPos, srcLen, (int) clamp(1, fLen, NumberOf(znBuff)), znBuff, &znLen, NumberOf(znBuff));
        logical beforeGMT = False;

        integer znPos;
        if (znBuff[0] == '-') {
          beforeGMT = True;
          znPos = 1;
        } else if (znBuff[0] == '+') {
          beforeGMT = False;
          znPos = 1;
        } else
          znPos = 0;

        int znHours = 0;
        int znMins = 0;

        ret = rdNum(znBuff, &znPos, znLen, 2, &znHours);

        if (znBuff[znPos] == ':')
          znPos++;

        if (ret == Ok)
          ret = rdNum(znBuff, &znPos, znLen, clamp(0, 2, znLen - znPos), &znMins);

        if (beforeGMT)
          gmtOffset = -znHours * 60 * 60 + znMins * 60;
        else
          gmtOffset = znHours * 60 * 60 + znMins * 60;
        continue;
      }
      default: { // skip over other characters
        codePoint ch = nextCodePoint(src, &srcPos, srcLen);
        if (ch != f)
          return Fail;
        continue;
      }
    }
  }

  if (isPm && hours < 12)
    hours += 12;
  time->tm_hour = hours;

  if (!isCE)
    time->tm_year = -year - 1900;
  else
    time->tm_year = year - 1900;

  time->tm_isdst = False;
  time->tm_mon = month;
  time->tm_mday = day;
  time->tm_min = mins;
  time->tm_sec = secs;
  time->tm_gmtoff = gmtOffset;
  return ret;
}

ReturnStatus g__parsetime(heapPo h, termPo a1, termPo a2) {
  integer srcLen;
  const char *src = strVal(a1, &srcLen);

  integer fmtLen;
  const char *fmt = strVal(a2, &fmtLen);

  struct tm time;

  retCode ret = parseTime(fmt, fmtLen, src, srcLen, &time);

  if (ret == Ok) {
    time_t tm = mktime(&time);
    return (ReturnStatus) {.ret=Normal, .result=(termPo) wrapSome(h, makeFloat((double) tm))};
  } else {
    return (ReturnStatus) {.ret=Normal, .result=noneEnum};
  }
}
