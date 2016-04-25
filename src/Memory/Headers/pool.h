/*
  Memory pool management
  (c) 1994-1999 Imperial College, F.G. McCabe  and Fujitsu Labs

  This program is free software; you can redistribute it and/or
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

#ifndef _POOL_H_
#define _POOL_H_

#include <sys/types.h>

/* Data structure pool management */
typedef struct _pool_ *poolPo;

poolPo newPool(size_t elsize, int initial);
logical emptyPool(poolPo base);

void *allocPool(poolPo pool); /* allocate an element from the pool */
void freePool(poolPo pool, void *el); /* free an element back to the pool */
void verifyPool(poolPo pool);

#endif
