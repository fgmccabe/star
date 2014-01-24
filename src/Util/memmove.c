/*
  Our own memmove function
  (c) 2014 F.G.McCabe

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
#ifndef HAVE_MEMMOVE
#include <stdlib.h>

void *memmove(void *dest,void *src,size_t n)
{
  register char *s1 = (char *)dest;
  register char *s2 = (char *)src;
  register long count = (long)n;

  if(s1>=s2 && s2+n>=s1){	/* They overlap? */
    s1+=count;
    s2+=count;

    while(count--)
      *--s1=*--s2;
  }
  else{
    while(count--)
      *s1++=*s2++;
  }
  return dest;
}
#endif
