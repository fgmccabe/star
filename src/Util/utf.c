/*
  Character encoding support functions
  (c) 1994-2004 Imperial College, F.G. McCabe and Fujitsu Labs

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.

  Contact: fgm@fla.fujitsu.com
*/

#include "config.h"		/* Invoke configuration header */
#include "io.h"
#include "utf.h"
#include "unicode.h"

// Implement the character incoding functions

retCode utf16OutChar(ioPo io,uniChar ch)
{
  retCode ret = preFlushFile(io,2);
  
  if(ret==Ok)
    ret = outByte(io,(ch>>8)&0xff);

  if(ret==Ok)
    ret = outByte(io,(ch&0xff));
  return ret;
}

retCode utf16SwapOutChar(ioPo io,uniChar ch)
{
  retCode ret = preFlushFile(io,2);
  
  if(ret==Ok)
    ret = outByte(io,(ch&0xff));
  if(ret==Ok)
    ret = outByte(io,(ch>>8)&0xff);
  return ret;
}

retCode utf8OutChar(ioPo io,uniChar ch)
{
  byte buff[8];
  retCode ret = preFlushFile(io,uniCharUtf8Size(ch));
  long len = uni_utf8(&ch,1,(unsigned char*)&buff[0],NumberOf(buff));

  if(ret==Ok)
    ret = outBytes(io,&buff[0],len,&len);
  return ret;
}

retCode rawOutChar(ioPo io,uniChar ch)
{
  retCode ret = preFlushFile(io,uniCharUtf8Size(ch));

  if(ret==Ok)
    ret = outByte(io,(byte)(ch&0xff));

  return ret;
}

retCode utf16InChar(ioPo io,uniChar *ch)
{
  byte hi,lo;
  retCode ret = inByte(io,&hi);

  if(ret==Ok){
    ret = inByte(io,&lo);

    if(ret==Ok)
      *ch = (uniChar)(hi<<8|lo);
    else
      putBackByte(io,hi);
  }

  return ret;
}

retCode utf16SwapInChar(ioPo io,uniChar *ch)
{
  byte hi,lo;
  retCode ret = inByte(io,&lo);

  if(ret==Ok){
    ret = inByte(io,&hi);

    if(ret==Ok)
      *ch = (uniChar)(hi<<8|lo);
    else
      putBackByte(io,lo);
  }
  return ret;
}

retCode utf8InChar(ioPo io,uniChar *ch)
{
  byte b;
  retCode ret = inByte(io,&b);

  if(ret==Ok){
    if(b<=0x7f){
      *ch = (uniChar)b;
      return Ok;
    }
    else if(UC80(b)){
      byte nb;
      ret = inByte(io,&nb);

      if(ret==Ok)
        *ch = (uniChar)(UX80(b)<<6|UXR(nb));
      else{
        putBackByte(io,b);         /* this will allow us to restart the inChar */
      }
      return ret;
    }
    else if(UC800(*ch)){
      byte up,md;

      ret = inByte(io,&md);
      if(ret==Ok){
        ret = inByte(io,&up);

        if(ret==Ok){
          *ch = (uniChar)((UX800(b)<<12)|(UXR(md)<<6) | (UXR(up)));
          return ret;
        }
        else{
          putBackByte(io,md);
          putBackByte(io,b);
          return ret;
        }
      }
      else{
        putBackByte(io,b);
        return ret;
      }
    }
    else{
      return Error;
    }
  }
  else
    return ret;
}

retCode rawInChar(ioPo io,uniChar *ch)
{
  byte b;
  retCode ret = inByte(io,&b);

  *ch = ((uniChar)b)&0xff;
  return ret;
}


