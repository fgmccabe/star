/*
  Hash Table Functions
  (c) 1994-2011 F.G. McCabe

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
*/

#include "config.h"		/* Invoke configuration header */
#include "retcode.h"
#include "logical.h"
#include "pool.h"
#include "hash.h"
#include "file.h"

#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


/* The structure of a hash table ... */
typedef struct bucket *bucketPo;

typedef struct bucket{
  void *nme;			/* The symbol in the hash table */
  void *r;			/* 'value' of the hashed record */
  bucketPo link;		/* Link in case of hash-crash */
} BucketRec;

typedef struct _hashtable_{
  long size;				/* The number of entries in the table */
  long entries;				/* how many entries do we have? */
  bucketPo *table;			/* The table of entries */
  hashFun hash;				/* The hashing function */
  compFun compare;			/* The comparison function */
  destFun destroy;			/* Entry destruction function */
  pthread_mutex_t mutex;		/* Mutex associated with table */
} HashTableRec;

static void rehash(hashPo tbl);	/* forward declarations */

static poolPo bpool = NULL;

/* Create a new table */
hashPo NewHash(long size,hashFun hash,compFun compare,destFun dest)
{
  long i;
  hashPo hp;

  hp = (hashPo)malloc(sizeof(HashTableRec));

  size = nextPrime(size);

  if(bpool==NULL)
    bpool = newPool(sizeof(BucketRec),1024);

  hp->size = size;
  hp->entries = 0;		/* this will be used to determine rehashing */
  hp->table = (bucketPo*)malloc(size*sizeof(bucketPo));
  if(hash==NULL)
    hp->hash = strhash;
  else
    hp->hash = hash;
  if(compare==NULL)
    hp->compare = strcomp;
  else
    hp->compare = compare;
  hp->destroy = dest;

  for(i=0;i<size;i++)
    hp->table[i]=NULL;

  initRecursiveMutex(&hp->mutex);

  return hp;
}

static retCode delBuckets(hashPo ht,bucketPo *b)
{
  bucketPo old;
  retCode stat=Ok;

  while(stat==Ok){
    if(*b==NULL)
      return Ok;
    else{
      old = *b;			/* unlink the bucket from the chain */
      *b = old->link;

      if(ht->destroy!=NULL && (stat=(ht->destroy)(old->nme,old->r))!=Ok){
	old->link = *b;
	*b = old;
	return stat;
      }

      freePool(bpool,old);
    }
  }
  return stat;
}

retCode DelHash(hashPo ht)
{
  retCode stat = Ok;
  register bucketPo *pp = ht->table;
  register int i=0;
  register int size = ht->size;

  pthread_mutex_lock(&ht->mutex);

  for(i=0;stat==Ok&&i<size;i++)
    if(pp[i]!=NULL)
      stat=delBuckets(ht,&pp[i]);

  if(stat!=Ok)
    return stat;
  
  pthread_mutex_unlock(&ht->mutex);
  pthread_mutex_destroy(&ht->mutex);
  free(ht->table);
  free(ht);

  return Ok;
}

/* Search the hash table */
void *Search(void *name,register hashPo htbl)
{
  return hashGet(htbl,name);
}

void *hashGet(hashPo htbl,void *name)
{
  void *result = NULL;
  pthread_mutex_lock(&htbl->mutex);

  {
    register bucketPo b = htbl->table[(htbl->hash)(name)%htbl->size]; 
  
    while(b!=NULL){
      if((htbl->compare)(b->nme,name)==0){ /* we have found the entry */
	result = b->r;
	break;
      }
      else
	b = b->link;		/* down the chain */
    }
  }

  pthread_mutex_unlock(&htbl->mutex);
  return result;
}

retCode Install(void *name, void *r,hashPo htbl)
{
  return hashPut(htbl,name,r);
}

/* Install in the hash table */
retCode hashPut(hashPo htbl,void *name, void *r)
{
  pthread_mutex_lock(&htbl->mutex);

  {
    register long offset = (htbl->hash)(name)%htbl->size;
    register bucketPo b = htbl->table[offset];

    while(b!=NULL){
      if((htbl->compare)(b->nme,name)==0){ /* we have found the entry */
	b->nme = name;
	b->r = r;
      
	pthread_mutex_unlock(&htbl->mutex);
	return Ok;		/* we found an old entry */
      }
      else
	b = b->link;		/* down the chain */
    }

    if(htbl->entries>=htbl->size){ /* table is full */
      rehash(htbl);
      pthread_mutex_unlock(&htbl->mutex);
      return Install(name,r,htbl);
    }
    else{
      b = allocPool(bpool);	/* create a new entry in the chain */
      b->nme = name;
      b->r = r;
      b->link = htbl->table[offset];
      htbl->table[offset]=b;
      htbl->entries++;
      
      pthread_mutex_unlock(&htbl->mutex);
      return Ok;
    }
  }
}

/* remove an entry from the hash table */
retCode Uninstall(void *name, hashPo htbl)
{
  return hashRemove(htbl,name);
}

retCode hashRemove(hashPo htbl,void *name)
{
  pthread_mutex_lock(&htbl->mutex);

  {
    register long offset = (htbl->hash)(name)%htbl->size;
    register bucketPo *b = &htbl->table[offset];

    while(*b!=NULL){
      if((htbl->compare)((*b)->nme,name)==0){ /* we have found the entry */
	bucketPo old = *b;
	retCode stat=Ok;
	
	*b = old->link;			/* remove the bucket from the table */

	if(htbl->destroy!=NULL && (stat=(htbl->destroy)(old->nme,old->r))!=Ok)
	  *b = old;			/* We don't destroy entry */
	else{
	  freePool(bpool,old);
	  htbl->entries--;		/* decrement number of entries */
	}

	pthread_mutex_unlock(&htbl->mutex);
	return stat;
      }
      else
	b = &(*b)->link;
    }
  }
  pthread_mutex_unlock(&htbl->mutex);

  return Fail;			/* not in the table... */
}

void lockHash(hashPo tbl)
{
  pthread_mutex_lock(&tbl->mutex);
}

void unlockHash(hashPo tbl)
{
  pthread_mutex_unlock(&tbl->mutex);
}

/* Rebuild the hash table when it has filled up */
/* Runs when the table mutex is locked */
static void rehash(hashPo tbl)
{
  long old_size = tbl->size;
  register long new_size = nextPrime(old_size*2);
  register long i;
  register bucketPo *old = tbl->table;

  tbl->size = new_size;

  if((tbl->table=(bucketPo*)calloc(new_size,sizeof(bucketPo)))==NULL){
    logMsg(logFile,"no space for hash table"); /* If not abort */
    exit(-1);
  }

  for(i=0;i<old_size;i++){
    if(old[i]!=NULL){
      bucketPo b = old[i];

      while(b!=NULL){
	register long offset = (tbl->hash)(b->nme)%tbl->size;
	register bucketPo bb = b; /* switch bucket from old table to new table */

	b = b->link;
	bb->link = tbl->table[offset];
	tbl->table[offset]=bb;
      }
    }
  }
  free(old);			/* release the old table */
}

/* Process the whole table
 * Additions and removals are permitted during this process
 * But a rehash is unsafe ... so insertions should be avoided
 */

retCode ProcessTable(procFun pr,hashPo htbl,void *c)
{
  retCode stat = Ok;

  pthread_mutex_lock(&htbl->mutex);

  {
    register int i;
    register int size = htbl->size;

    for(i=0;stat==Ok && i<size;i++){
      if(htbl->table[i]!=NULL){
	bucketPo b = htbl->table[i];
	while(stat==Ok && b!=NULL){
	  BucketRec B = *b;
	  
	  stat=pr(B.nme, B.r, c);
	  b = b->link;
	}
      }
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return stat;
}

logical verifyHash(hashPo htbl)
{
  logical stat = True;

  pthread_mutex_lock(&htbl->mutex);

  {
    register int i;
    register int size = htbl->size;

    for(i=0;stat && i<size;i++){
      if(htbl->table[i]!=NULL){
	bucketPo b = htbl->table[i];
	while(stat && b!=NULL){
	  if(Search(b->nme,htbl)!=b->r)
	    stat = False;
	  
	  b = b->link;
	}
      }
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return stat;
}


long hashSize(hashPo htbl)
{
  long count = 0;

  pthread_mutex_lock(&htbl->mutex);

  {
    register int i;
    register int size = htbl->size;

    for(i=0;i<size;i++){
      if(htbl->table[i]!=NULL)
	count++;
    }
  }

  pthread_mutex_unlock(&htbl->mutex);

  return count;
}

/* Use the sieve of Erastosthenes to find the next prime that is larger 
   than a given number
   Not the fastest algorithm, but it isnt called very often
*/

   
typedef struct _primeHopper_ *primePo;

typedef struct _primeHopper_ {
  integer pr;
  primePo next;
} primeHopper;

// Not too pretty, but hey who's going to see?

static poolPo prpool = NULL;
static primePo primes = NULL;
static pthread_mutex_t prMutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t prOnce = PTHREAD_ONCE_INIT;

static void initPrimes(void)
{
  if(prpool==NULL){
    prpool = newPool(sizeof(primeHopper),1024);
    primes = (primePo)allocPool(prpool);
    
    primes->pr = 3;			/* We know that 3 is a prime! */
    primes->next = NULL;
  }
}

integer nextPrime(integer min)
{
  integer candidate = 0;

  pthread_once(&prOnce,initPrimes);
  pthread_mutex_lock(&prMutex);

  {
    primePo soFar = primes;
    
    while(candidate<=min && soFar!=NULL){ // Look in the table we have so far
      candidate = soFar->pr;
      soFar = soFar->next;
    }

    while(candidate<=min){	// We have to extend the table
      candidate+=2;
    
    again:
      for(soFar=primes;soFar!=NULL;soFar=soFar->next){
	if(candidate%soFar->pr==0){
	  candidate+=2;
	  goto again;
	}
	else if(soFar->next==NULL){	// We have a new prime
	  primePo new = (primePo)allocPool(prpool);
        
	  new->pr = candidate;
	  new->next = NULL;
	  soFar->next = new;
	  break;
	}
      }
    }
  }

  pthread_mutex_unlock(&prMutex);
  
  return candidate;
}


/* compute the hash value of a string .. */
/* This function is used as a default if none is supplied when the 
   hash table is created 
*/
uinteger strhash(void *n)
{
  register char *name = (char*)n;
  register uinteger hash = 0;

  if(name)
    while(*name)
      hash = hash*37+*name++;

  return hash;
}

int strcomp(void *n1,void *n2)
{
  return strcmp(n1,n2);
}

