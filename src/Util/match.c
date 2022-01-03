//
// Created by Francis McCabe on 1/1/22.
//

#include "match.h"

static retCode match(char *src, integer len, integer pos, char *srch, integer slen, integer spos);

logical globMatch(char *src, integer len, char *srch, integer slen) {
  return match(src, len, 0, srch, slen, 0) == Ok;
}

logical strMatch(char *src, char *srch) {
  return globMatch(src, uniStrLen(src), srch, uniStrLen(srch));
}

retCode match(char *src, integer len, integer pos, char *srch, integer slen, integer spos) {
  while (pos < len) {
    codePoint m = nextCodePoint(srch, &spos, slen);

    if (m == '*') {
      while (spos < slen && codePointAt(srch, spos, slen) == '*') {
        nextCodePoint(src, &spos, slen);
      }
      if (spos == slen)
        return Ok;
      else {
        retCode ret = Ok;
        while (pos < len && (ret = match(src, len, pos, srch, slen, spos)) == Fail) {
          nextCodePoint(src,&pos,len);
        }
        if (ret != Ok)
          return Error;
        else if (pos < len)
          return Ok;
        else
          return Fail;
      }
    } else{
      codePoint ch = nextCodePoint(src, &pos, len);
      if (m == '?' || m == ch)
        continue;
      else
        return Fail;
    }
  }
  while (spos < slen && codePointAt(srch, spos, slen) == '*')
    spos++;
  if (spos == slen)
    return Ok;
  else
    return Fail;
}
