//
// Created by Francis McCabe on 2/1/17.
//

#include "base64.h"

static char codes[];

static byte decByte(byte ch);

// Decode a stream of characters as base64
retCode decode64(ioPo dst, ioPo src) {
  retCode ret = Ok;

  while (ret == Ok) {
    byte u;

    ret = inByte(src, &u);

    if (ret == Eof)           // Eof allowed on first character
      return Ok;
    else if (ret == Ok) {
      byte h;

      ret = inByte(src, &h);

      if (ret == Ok) {
        byte m;

        ret = inByte(src, &m);

        if (ret == Ok) {
          if (m == '=') {
            byte l;

            ret = inByte(src, &l);

            if (ret == Ok && l == '=') {
              byte Up = decByte(u);
              byte Hi = decByte(h);
              uint32 w = (Up << 18) | (Hi << 12);
              return outByte(dst, (byte) ((w >> 16) & 255));
            } else if (ret == Eof)
              return Eof;
            else
              return Error;
          } else {
            byte l;

            ret = inByte(src, &l);

            if (ret == Ok) {
              if (l == '=') {
                byte Up = decByte(u);
                byte Hi = decByte(h);
                byte Lo = decByte(m);
                uint32 w = (Up << 18) | (Hi << 12) | Lo << 6;
                byte b1 = (byte) ((w >> 16) & 255);
                byte b2 = (byte) ((w >> 8) & 255);

                ret = outByte(dst, b1);
                if (ret == Ok)
                  ret = outByte(dst, b2);
                return ret;
              } else {
                byte Up = decByte(u);
                byte Hi = decByte(h);
                byte Md = decByte(m);
                byte Lo = decByte(l);
                uint32 w = (Up << 18) | (Hi << 12) | Md << 6 | Lo;
                byte b1 = (byte) ((w >> 16) & 255);
                byte b2 = (byte) ((w >> 8) & 255);
                byte b3 = (byte) (w & 255);

                ret = outByte(dst, b1);
                if (ret == Ok)
                  ret = outByte(dst, b2);
                if (ret == Ok)
                  ret = outByte(dst, b3);
              }
            }
          }
        }

      }
    }
  }
  return ret;
}

static retCode encodeWord(ioPo dst, uint32 val);
static void map2Six(uint32 val, byte *up, byte *hi, byte *md, byte *lo);
static retCode encodeLast(ioPo dst, byte hi, byte md, byte lo, char *rest);
static retCode encByte(ioPo dst, byte b);

// Encode a stream of bytes into base64 characters
retCode encode64(ioPo src, ioPo dst) {
  retCode ret = Ok;

  while (ret == Ok) {
    byte h;
    byte Up, Hi, Md, Lo;

    ret = inByte(src, &h);

    if (ret == Ok) {
      byte m;

      ret = inByte(src, &m);

      if (ret == Ok) {
        byte l;
        ret = inByte(src, &l);

        if (ret == Ok) {
          ret = encodeWord(dst, (((unsigned int) h & 0xff) << 16) | ((m & 0xff) << 8) | (l & 0xff));
        } else if (ret == Eof) {
          map2Six((((unsigned int) h & 0xff) << 16) | ((m & 0xff) << 8), &Up, &Hi, &Md, &Lo);
          return encodeLast(dst, Up, Hi, Md, "=");
        }
      } else if (ret == Eof) {
        map2Six((((unsigned int) h & 0xff) << 16), &Up, &Hi, &Md, &Lo);
        return encodeLast(dst, Up, Hi, Md, "==");
      }
    } else
      return ret;
  }
  return ret;
}

static void map2Six(uint32 val, byte *up, byte *hi, byte *md, byte *lo) {
  *up = (byte) ((val >> 18) & 63);
  *hi = (byte) ((val >> 12) & 63);
  *md = (byte) ((val >> 6) & 63);
  *lo = (byte) (val & 63);
}

static retCode encodeWord(ioPo dst, uint32 val) {
  byte Up, Hi, Md, Lo;

  map2Six(val, &Up, &Hi, &Md, &Lo);

  retCode ret = encByte(dst, Up);
  if (ret == Ok)
    ret = encByte(dst, Hi);
  if (ret == Ok)
    ret = encByte(dst, Md);
  if (ret == Ok)
    ret = encByte(dst, Lo);
  return ret;
}

static retCode encodeLast(ioPo dst, byte hi, byte md, byte lo, char *rest) {
  retCode ret = encByte(dst, hi);
  if (ret == Ok)
    ret = encByte(dst, md);
  if (ret == Ok && lo != 0)
    ret = encByte(dst, lo);
  if (ret == Ok)
    ret = outStr(dst, rest);
  return ret;
}

static retCode encByte(ioPo dst, byte b) {
  return outByte(dst, (byte) codes[b]);
}

// Core table for encoding
static char codes[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
  'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
  'x', 'y', 'z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
};

static byte decByte(byte code) {
  for (int ix = 0; ix < NumberOf(codes); ix++) {
    if (codes[ix] == code)
      return (byte) ix;
  }
  return (byte) -1;
}
