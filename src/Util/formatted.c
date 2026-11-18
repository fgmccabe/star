//
// Created by Francis McCabe on 11/26/21.
//
#include "formatted.h"

static integer countSignificants(const char *frmt, integer limit, const char *test);

#define attachChar(O, P, L, Ch) do{ if((*(P))>=(L)) return Error; else (O)[(*(P))++] = Ch; } while(False)

retCode
formatDigits(sign sign, const char *digits, integer digitLen, integer precision, const char *format, integer formatLen,
             char *out, integer outLen, integer *pos) {
  integer formSigDigits = countSignificants(format, formatLen, "09X ");
  logical encounteredSign = False;
  integer zeroDigits = countSignificants(format, formatLen, "0 ");

  char signFmt = (char) (uniIndexOf(format, formatLen, 0, '-') >= 0 ? '-' :
                         uniIndexOf(format, formatLen, 0, '+') >= 0 ? '+' : ' ');
  char buff[outLen];

  *pos = 0;

  for (integer ix = formatLen - 1, px = minimum(formSigDigits,precision)- 1; ix >= 0; ix--) {
    char formChar = format[ix];
    switch (formChar) {
      case '-':
        if (!encounteredSign) {
          encounteredSign = True;
          switch (sign) {
            case positive:
            default:
              attachChar(buff, pos, outLen, ' ');
              break;
            case negative:
              attachChar(buff, pos, outLen, '-');
              break;
          }
        } else {
          attachChar(buff, pos, outLen, ' ');
        }
        break;
      case '+':
        if (!encounteredSign) {
          encounteredSign = True;
          switch (sign) {
            case positive:
            default:
              attachChar(buff, pos, outLen, '+');
              break;
            case negative:
              attachChar(buff, pos, outLen, '-');
              break;
          }
        } else {
          attachChar(buff, pos, outLen, ' ');
        }
        break;
      case '(':
        if (sign == negative) {
          attachChar(buff, pos, outLen, '(');
        } else
          attachChar(buff, pos, outLen, ' ');

        break;
      case ')':
        if (sign == negative) {
          attachChar(buff, pos, outLen, ')');
        } else
          attachChar(buff, pos, outLen, ' ');

        break;
      case '.':
      case ',':
      default:
        if (px >= 0 || zeroDigits > 0)
          attachChar(buff, pos, outLen, formChar);
        break;
      case ' ':
        if (px >= 0) {
          // more of the raw result to write buff
          attachChar(buff, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0) {
          switch (signFmt) {
            case '-':
              if (sign == negative && !encounteredSign) {
                encounteredSign = True;
                attachChar(buff, pos, outLen, '-');
                sign = positive;
                signFmt = ' ';
              } else {
                attachChar(buff, pos, outLen, ' ');
              }
              break;
            case '+': {
              if (!encounteredSign) {
                encounteredSign = True;
                if (sign == positive) {
                  attachChar(buff, pos, outLen, '+');
                } else {
                  attachChar(buff, pos, outLen, '-');
                }
              } else {
                attachChar(buff, pos, outLen, ' ');
              }
              signFmt = ' ';
              break;
            }
            default:
              attachChar(buff, pos, outLen, ' ');
              break;
          }
        }

        zeroDigits--;
        break;
      case '0':
        if (px >= 0) {
          // more of the raw result to write out
          attachChar(buff, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0)
          attachChar(buff, pos, outLen, '0');

        zeroDigits--;
        break;
      case '9':
      case 'X':
        if (px >= 0) {
          // more of the raw result to write out
          attachChar(buff, pos, outLen, digits[px]);
          px--;
        }
        break;
      case 'U':
        if (px >= 0) {
        }
      case 'e':
      case 'E':
      case 'L':
      case 'R':
        return Error;
    }
  }

  return uniReverse(buff, out, *pos);
}

integer countSignificants(const char *frmt, integer limit, const char *test) {
  integer cx = 0;
  integer tLen = uniStrLen(test);
  for (integer ix = 0; ix < limit;) {
    codePoint ch = nextCodePoint(frmt, &ix, limit);

    if (uniIndexOf(test, tLen, 0, ch) >= 0)
      cx++;
  }
  return cx;
}

