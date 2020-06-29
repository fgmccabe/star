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
integer parseInt(const char *s, integer l) {
  integer x;
  parseInteger(s, l, &x);
  return x;
}

retCode parseInteger(const char *s, integer len, integer *res) {
  logical positive = True;
  integer x = 0;
  integer pos = 0;

  if (*s == '-') {
    positive = False;
    pos++;
  } else if (*s == '+') {
    positive = True;
    pos++;
  }
  while (pos < len) {
    codePoint ch = nextCodePoint(s, &pos, len);
    if (isNdChar(ch)) {
      x = x * 10 + digitValue(ch);
    } else if (!isSpaceChar(ch))
      return Error;
  }

  if (!positive)
    *res = -x;
  else
    *res = x;
  return Ok;
}

double parseNumber(char *s, integer l) {
  double x;

  parseDouble(s, l, &x);
  return x;
}

retCode parseDouble(const char *s, integer l, double *rslt) {
  logical positive = True;
  double x = 0;
  int exp = 0;
  int digit;
  integer pos = 0;

  if (s[pos] == '-') {
    positive = False;
    pos++;
  } else if (s[pos] == '+') {
    positive = True;
    pos++;
  }
  while (pos < l && (digit = digitValue((codePoint) s[pos])) >= 0) {
    x = x * 10 + digit;
    pos++;
  }

  if (pos < l && s[pos] == '.') {
    double power = 0.1;
    pos++;

    while (pos < l && (digit = digitValue((codePoint) s[pos])) >= 0) {
      pos++;
      x = x + digit * power;
      power /= 10;
    }

    if (pos < l && (s[pos] == 'e' || s[pos] == 'E')) {
      logical eSign = True;
      pos++;

      if (pos < l && (s[pos] == '-')) {
        eSign = False;
        pos++;
      } else if (pos < l && s[pos] == '+') {
        pos++;
      }

      while (pos < l && (digit = digitValue((codePoint) s[pos])) >= 0) {
        pos++;
        exp = exp * 10 + digit;
      }

      if (!eSign)
        exp = -exp;
    }
  }

  if (positive)
    *rslt = x * pow(10, exp);
  else
    *rslt = -x * pow(10, exp);

  while (pos < l && isSpaceChar((codePoint) s[pos]))
    pos++;
  if (pos == l)
    return Ok;
  else
    return Error;
}

