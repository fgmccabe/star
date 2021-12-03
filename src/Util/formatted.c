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

  if (precision > formSigDigits)
    return Error;

  *pos = 0;

  for (integer ix = formatLen - 1, px = precision - 1; ix >= 0; ix--) {
    char formChar = format[ix];
    switch (formChar) {
      case '-':
        switch (sign) {
          case positive:
          default:
            attachChar(out, pos, outLen, ' ');
            break;
          case negative:
            attachChar(out, pos, outLen, '-');
            break;
        }
        break;
      case '+':
        switch (sign) {
          case positive:
          default:
            attachChar(out, pos, outLen, '+');
            break;
          case negative:
            attachChar(out, pos, outLen, '-');
            break;
        }
        break;
      case '(':
        if (sign == negative) {
          attachChar(out, pos, outLen, '(');
        } else
          attachChar(out, pos, outLen, ' ');

        break;
      case ')':
        if (sign == negative) {
          attachChar(out, pos, outLen, ')');
        } else
          attachChar(out, pos, outLen, ' ');

        break;
      case '.':
      case ',':
      default:
        if (px >= 0 || zeroDigits > 0)
          attachChar(out, pos, outLen, formChar);
        break;
      case ' ':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0) {
          switch (signFmt) {
            case '-':
              if (sign == negative) {
                attachChar(out, pos, outLen, '-');
                sign = positive;
                signFmt = ' ';
              } else {
                attachChar(out, pos, outLen, ' ');
              }
              break;
            case '+': {
              if (sign == positive) {
                attachChar(out, pos, outLen, '-');
              } else {
                attachChar(out, pos, outLen, '+');
              }
              signFmt = ' ';
              break;
            }
            default:
              attachChar(out, pos, outLen, ' ');
              break;
          }
        }

        zeroDigits--;
        break;
      case '0':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0)
          attachChar(out, pos, outLen, '0');

        zeroDigits--;
        break;
      case '9':
      case 'X':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
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

  return uniReverse(out, *pos);
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

