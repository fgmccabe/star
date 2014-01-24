/***********************************************************************
(c) 1994-2010 Imperial College, F.G. McCabe

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

Contact: frankmccabe@mac.com
***********************************************************************/
#ifndef _LIB_HASH_H_
#define _LIB_HASH_H_

#include "retcode.h"
#include "integer.h"

/* Hash table interface */
typedef struct _hashtable_ *hashPo;

typedef uinteger (*hashFun)(void *); /* Hashing function */
typedef int (*compFun)(void *, void *); /* Comparison function */
typedef retCode (*destFun)(void *, void *); /* Destroy function */
typedef retCode (*procFun)(void *n, void *r, void *c); /* Processing func */

integer nextPrime(integer min);

/* Build a new hash table */
hashPo NewHash(long size,hashFun hash,compFun cmp,destFun dest);
retCode DelHash(hashPo hp);
retCode ProcessTable(procFun pr,hashPo tbl,void *c);

// Deprecated
retCode Install(void *name, void *r,hashPo htbl); /* install a new entry */
void *Search(void *name,hashPo htbl); /* search for an entry */
retCode Uninstall(void *name,hashPo htbl); /* remove an entry from the hash table */

// Use these instead
retCode hashPut(hashPo htbl,void *name, void *r); // install a new entry
void *hashGet(hashPo htbl,void *name); // search for an entry
retCode hashRemove(hashPo htbl,void *name); // remove an entry from the hash tabl
long hashSize(hashPo htbl);

void lockHash(hashPo tbl);
void unlockHash(hashPo tbl);

uinteger strhash(void *name);
int strcomp(void *n1,void *n2);

#endif
