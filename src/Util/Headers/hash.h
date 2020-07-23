/*
  HashTable header
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _LIB_HASH_H_
#define _LIB_HASH_H_

#include "retcode.h"
#include "integer.h"

/* Hash table interface */
typedef struct hashtable_ *hashPo;

typedef integer (*hashFun)(void *); /* Hashing function */
typedef comparison (*compFun)(void *, void *); /* Comparison function */
typedef retCode (*destFun)(void *, void *); /* Destroy function */
typedef retCode (*procFun)(void *n, void *r, void *c); /* Processing func */

/* Build a new hash table */
hashPo newHash(long size, hashFun hash, compFun cmp, destFun dest);
retCode eraseHash(hashPo hp);
retCode ProcessTable(procFun pr,hashPo tbl,void *c);

// Use these instead
retCode hashPut(hashPo htbl,void *name, void *r); // install a new entry
void *hashGet(hashPo htbl,void *name); // search for an entry
retCode hashRemove(hashPo htbl,void *name); // remove an entry from the hash tabl
integer hashSize(hashPo htbl);

void lockHash(hashPo tbl);
void unlockHash(hashPo tbl);

integer strhash(void *name);
comparison strcomp(void *n1,void *n2);

integer ixHash(void *r);
comparison ixCmp(void *i1, void *i2);

#endif
