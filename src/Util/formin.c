/*
  Input scanning functions for I/O library
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "io.h"
#include "formioP.h"

#include <math.h>

/* 
 * Scan a unicode string, looking for a number of a given base
 */
integer parseInteger(char *s, integer l) {
  logical positive = True;
  integer x = 0;
  int digit;

  if (*s == '-') {
    positive = False;
    s++;
    l--;
  } else if (*s == '+') {
    positive = True;
    s++;
    l--;
  }
  while (l > 0 && (digit = digitValue((codePoint)(*s++))) >= 0) {
    l--;
    x = x * 10 + digit;
  }
  if (positive)
    return x;
  else
    return -x;
}

double parseNumber(char *s, long l) {
  logical positive = True;
  double x = 0;
  int exp = 0;
  int digit;

  if (*s == '-') {
    positive = False;
    s++;
    l--;
  } else if (*s == '+') {
    positive = True;
    s++;
    l--;
  }
  while (l > 0 && (digit = digitValue((codePoint)(*s))) >= 0) {
    l--;
    x = x * 10 + digit;
    s++;
  }

  if (l > 0 && *s == '.') {
    double power = 0.1;
    s++;
    l--;

    while (l > 0 && (digit = digitValue((codePoint)(*s))) >= 0) {
      l--;
      s++;
      x = x + digit * power;
      power /= 10;
    }

    if (l > 0 && (*s == 'e' || *s == 'E')) {
      logical eSign = True;
      l--;
      s++;

      if (l > 0 && (*s == '-')) {
        eSign = False;
        l--;
        s++;
      } else if (l > 0 && *s == '+') {
        l--;
        s++;
      }

      while (l > 0 && (digit = digitValue((codePoint)(*s))) >= 0) {
        l--;
        s++;
        exp = exp * 10 + digit;
      }

      if (!eSign)
        exp = -exp;
    }
  }

  if (positive)
    return x * pow(10, exp);
  else
    return -x * pow(10, exp);
}

retCode lookingAt(ioPo in, char *test) {
  integer mark;
  retCode ret = markIo(in, &mark);

  if (ret == Ok) { // Can we mark this stream?
    while (*test != 0) {
      codePoint ch;
      ret = inChar(in, &ch);
      if (ret == Ok && ch == *test++)
        continue;
      else if (ret == Ok) {
        resetToMark(in, mark);
        return Fail;
      } else {
        resetToMark(in, mark);
        return ret;
      }
    }
    if (*test != 0)
      resetToMark(in, mark);
    return ret;
  }

  return ret;
}
