/*
  Our own memcmp function
  (c) 1994-1999 Imperial College and F.G.McCabe

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

  This function is compiled only if the system does not have a memory move
  function
  An important constraint of this is that it should work even if the 
  memory ranges overlap

 */
#include "config.h"
#ifndef HAVE_MEMCMP
#include <stdlib.h>

int memcmp(const void *S1,const void *S2,size_t n)
{
  register char *s1 = (char *)S1;
  register char *s2 = (char *)S2;
  register long count = (long)n;

  while(count-->0){
    if(*s1>*s2)
      return -1;
    else if(*s1<*s2)
      return 1;
    else{
      s1++; s2++;
    }
  }
  return 0;
}

#endif
