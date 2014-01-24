/* 
   High level I/O handling functions
   (c) 1994-2000 Imperial College and F.G. McCabe

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
   Contact: Francis McCabe <fgm@fla.fujitsu.com>
*/ 

#ifndef _IO_FORMIO_H_
#define _IO_FORMIO_H_

#include "io.h"

retCode int2Uni(integer i,int base,logical sign,uniChar *buff,long len);
retCode outInteger(ioPo f,long long i,int base,int width,int precision,
		     uniChar pad,logical left,char *prefix,logical sign);
retCode outDouble(ioPo out,double x,char mode,int width,int precision,
		    uniChar pad,logical left,char *prefix,logical sign);
retCode outMsg(ioPo f,char *fmt,...);
retCode logMsg(ioPo out,char *fmt,...);

retCode outUniString(ioPo f,uniChar *str,int len,int width,int precision,
		     uniChar pad,logical leftPad,logical alt);
retCode outInt(ioPo f,integer i);
retCode outFloat(ioPo out,double x);
retCode outUStr(ioPo f,uniChar *str);

integer parseInteger(uniChar *s,long len);
double  parseNumber(uniChar *s,long len);

#endif
