/*
  Hash Table Functions

  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "retcode.h"
#include "logical.h"
#include "pool.h"
#include "hash.h"
#include "file.h"
#include "formio.h"

#include <stdlib.h>

/* The structure of a hash table ... */
typedef struct bucket *bucketPo;

typedef struct bucket {
  void *nme;
  /* The symbol in the hash table */
  void *r;
  /* 'value' of the hashed record */
  bucketPo link;    /* Link in case of hash-crash */
} BucketRec;

typedef struct _hashtable_ {
  integer size; // The number of entries in the table
  integer entries; /* how many entries do we have? */
  bucketPo *table; /* The table of entries */
  hashFun hash; /* The hashing function */
  compFun compare; /* The comparison function */
  destFun destroy; /* Entry destruction function */
  pthread_mutex_t mutex;    /* Mutex associated with table */
} HashTableRec;

static void rehash(hashPo tbl);  // forward declarations */

static poolPo bpool = NULL;

/* Create a new table */
hashPo NewHash(long size, hashFun hash, compFun compare, destFun dest) {
  long i;
  hashPo hp;

  hp = (hashPo) malloc(sizeof(HashTableRec));

  size = nextPrime(size);

  if (bpool == NULL)
    bpool = newPool(sizeof(BucketRec), 1024);

  hp->size = size;
  hp->entries = 0;    /* this will be used to determine rehashing */
  hp->table = (bucketPo *) malloc(size * sizeof(bucketPo));
  if (hash == NULL)
    hp->hash = strhash;
  else
    hp->hash = hash;
  if (compare == NULL)
    hp->compare = strcomp;
  else
    hp->compare = compare;
  hp->destroy = dest;

  for (i = 0; i < size; i++)
    hp->table[i] = NULL;

  initRecursiveMutex(&hp->mutex);

  return hp;
}

static retCode delBuckets(hashPo ht, bucketPo *b) {
  bucketPo old;
  retCode stat = Ok;

  while (stat == Ok) {
    if (*b == NULL)
      return Ok;
    else {
      old = *b;      /* unlink the bucket from the chain */
      *b = old->link;

      if (ht->destroy != NULL && (stat = (ht->destroy)(old->nme, old->r)) != Ok) {
        old->link = *b;
        *b = old;
        return stat;
      }

      freePool(bpool, old);
    }
  }
  return stat;
}

retCode DelHash(hashPo ht) {
  retCode stat = Ok;
  register bucketPo *pp = ht->table;
  register long i = 0;
  register long size = ht->size;

  pthread_mutex_lock(&ht->mutex);

  for (i = 0; stat == Ok && i < size; i++)
    if (pp[i] != NULL)
      stat = delBuckets(ht, &pp[i]);

  if (stat != Ok)
    return stat;

  pthread_mutex_unlock(&ht->mutex);
  pthread_mutex_destroy(&ht->mutex);
  free(ht->table);
  free(ht);

  return Ok;
}

/* Search the hash table */
void *hashGet(hashPo htbl, void *name) {
  void *result = NULL;
  pthread_mutex_lock(&htbl->mutex);

  {
    register integer offset = labs((htbl->hash)(name) % htbl->size);
    register bucketPo b = htbl->table[offset];

    while (b != NULL) {
      if ((htbl->compare)(b->nme, name) == same) { /* we have found the entry */
        result = b->r;
        break;
      } else
        b = b->link;    /* down the chain */
    }
  }

  pthread_mutex_unlock(&htbl->mutex);
  return result;
}

/* Install in the hash table */
retCode hashPut(hashPo htbl, void *name, void *r) {
  pthread_mutex_lock(&htbl->mutex);

  {
    register integer offset = labs((htbl->hash)(name) % htbl->size);
    register bucketPo b = htbl->table[offset];

    while (b != NULL) {
      if ((htbl->compare)(b->nme, name) == same) { /* we have found the entry */
        if (htbl->destroy != Null)
          htbl->destroy(b->nme, b->r);
        b->nme = name;
        b->r = r;

        pthread_mutex_unlock(&htbl->mutex);
        return Ok;    /* we found an old entry */
      } else
        b = b->link;    /* down the chain */
    }

    if (htbl->entries >= htbl->size) { /* table is full */
      rehash(htbl);
      pthread_mutex_unlock(&htbl->mutex);
      return hashPut(htbl, name, r);
    } else {
      b = allocPool(bpool);  /* create a new entry in the chain */
      b->nme = name;
      b->r = r;
      b->link = htbl->table[offset];
      htbl->table[offset] = b;
      htbl->entries++;

      pthread_mutex_unlock(&htbl->mutex);
      return Ok;
    }
  }
}

/* remove an entry from the hash table */

retCode hashRemove(hashPo htbl, void *name) {
  pthread_mutex_lock(&htbl->mutex);

  {
    register integer offset = labs((htbl->hash)(name) % htbl->size);
    register bucketPo *b = &htbl->table[offset];

    while (*b != NULL) {
      if ((htbl->compare)((*b)->nme, name) == 0) { /* we have found the entry */
        bucketPo old = *b;
        retCode stat = Ok;

        *b = old->link;      /* remove the bucket from the table */

        if (htbl->destroy != NULL && (stat = (htbl->destroy)(old->nme, old->r)) != Ok)
          *b = old;      /* We don't destroy entry */
        else {
          freePool(bpool, old);
          htbl->entries--;    /* decrement number of entries */
        }

        pthread_mutex_unlock(&htbl->mutex);
        return stat;
      } else
        b = &(*b)->link;
    }
  }
  pthread_mutex_unlock(&htbl->mutex);

  return Fail;      /* not in the table... */
}

void lockHash(hashPo tbl) {
  pthread_mutex_lock(&tbl->mutex);
}

void unlockHash(hashPo tbl) {
  pthread_mutex_unlock(&tbl->mutex);
}

/* Rebuild the hash table when it has filled up */
/* Runs when the table mutex is locked */
static void rehash(hashPo tbl) {
  long old_size = tbl->size;
  register integer new_size = nextPrime(old_size * 2);
  register long i;
  register bucketPo *old = tbl->table;

  tbl->size = new_size;

  if ((tbl->table = (bucketPo *) calloc((size_t)new_size, sizeof(bucketPo))) == NULL) {
    logMsg(logFile, "no space for hash table"); /* If not abort */
    exit(-1);
  }

  for (i = 0; i < old_size; i++) {
    if (old[i] != NULL) {
      bucketPo b = old[i];

      while (b != NULL) {
        register long offset = labs((tbl->hash)(b->nme) % tbl->size);
        register bucketPo bb = b; /* switch bucket from old table to new table */

        b = b->link;
        bb->link = tbl->table[offset];
        tbl->table[offset] = bb;
      }
    }
  }
  free(old);      /* release the old table */
}

/* Process the whole table
 * Additions and removals are permitted during this process
 * But a rehash is unsafe ... so insertions should be avoided
 */

retCode ProcessTable(procFun pr, hashPo htbl, void *c) {
  retCode stat = Ok;

  pthread_mutex_lock(&htbl->mutex);

  {
    register int i;
    register long size = htbl->size;

    for (i = 0; stat == Ok && i < size; i++) {
      if (htbl->table[i] != NULL) {
        bucketPo b = htbl->table[i];
        while (stat == Ok && b != NULL) {
          BucketRec B = *b;

          stat = pr(B.nme, B.r, c);
          b = b->link;
        }
      }
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return stat;
}

logical verifyHash(hashPo htbl) {
  logical stat = True;

  pthread_mutex_lock(&htbl->mutex);

  {
    register long size = htbl->size;

    for (long i = 0; stat && i < size; i++) {
      if (htbl->table[i] != NULL) {
        bucketPo b = htbl->table[i];
        while (stat && b != NULL) {
          if (hashGet(htbl, b->nme) != b->r)
            stat = False;

          b = b->link;
        }
      }
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return stat;
}

integer hashSize(hashPo htbl) {
  integer count = 0;

  pthread_mutex_lock(&htbl->mutex);

  {
    register int i;
    register integer size = htbl->size;

    for (i = 0; i < size; i++) {
      if (htbl->table[i] != NULL)
        count++;
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return count;
}


/* compute the hash value of a char * .. */
/* This function is used as a default if none is supplied when the 
   hash table is created 
*/
integer strhash(void *n) {
  register char *name = (char *) n;
  register integer hash = 0;

  if (name)
    while (*name)
      hash = hash * 37 + *name++;

  return hash;
}

comparison strcomp(void *n1, void *n2) {
  return uniCmp((char *) n1, (char *) n2);
}

integer ixHash(void *r) {
  return (integer) r;
}

comparison integerCmp(void *i1, void *i2){
  integer ix1 = (integer)i1;
  integer ix2 = (integer)i2;

  if(ix1<ix2)
    return smaller;
  else if(ix1>ix2)
    return bigger;
  else
    return same;
}

