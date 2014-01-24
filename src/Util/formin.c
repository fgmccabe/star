/*
  Input scanning functions for I/O library
  (c) 1994-2000 Imperial College, F.G. McCabe  and Fujitsu Labs

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
#include "formioP.h"

#include <math.h>

/* 
 * Scan a unicode string, looking for a number of a given base
 */
integer parseInteger(uniChar *s,long l)
{
  logical positive = True;
  integer x = 0;
  int digit;

  if(*s=='-'||*s==0x2212){
    positive = False;
    s++;
    l--;
  }
  else if(*s=='+'){
    positive = True;
    s++;
    l--;
  }
  while(l>0 && (digit=digitValue(*s++))>=0){
    l--;
    x = x*10+digit;
  }
  if(positive)
    return x;
  else
    return -x;
}

double parseNumber(uniChar *s,long l)
{
  logical positive = True;
  number x = 0;
  int exp = 0;
  int digit;

  if(*s=='-'||*s==0x2212){
    positive = False;
    s++;
    l--;
  }
  else if(*s=='+'){
    positive = True;
    s++;
    l--;
  }
  while(l>0 && (digit=digitValue(*s))>=0){
    l--;
    x = x*10+digit;
    s++;
  }
  
  if(l>0 && *s=='.'){
    number power = 0.1;
    s++; l--;
    
    while(l>0 && (digit=digitValue(*s))>=0){
      l--;    s++;
      x=x+digit*power;
      power /=10;
    }
    
    if(l>0 && (*s=='e'||*s=='E')){
    	logical eSign=True;
    	l--; s++;
    	
    	if(l>0 && (*s=='-'||*s==0x2212)){
    	  eSign=False; l--; s++;
    	}
    	else if(l>0 && *s=='+'){
    	  l--; s++;
    	}
    	
    	while(l>0 && (digit=digitValue(*s))>=0){
          l--; s++;
          exp = exp*10+digit;
    	}
    	
    	if(!eSign)
    	  exp = -exp;
    }
  }

  if(positive)
    return x*pow(10,exp);
  else
    return -x*pow(10,exp);
}


