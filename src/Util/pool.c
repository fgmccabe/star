/*
  Memory pool management
  (c) 1994-2000 Imperial College, F.G. McCabe  and Fujitsu Labs

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

/*
 * Generic element pool handling
 * Implements pools of objects that can be allocated and freed
 */
#include "config.h"		/* Invoke configuration header */
#include "retcode.h"
#include "logical.h"
#include "pool.h"
#include "file.h"

#include <stdlib.h>		/* access malloc etc. */
#include <assert.h>

#if DMALLOC
#include <dmalloc.h>		/* debugging malloc library */
#endif

typedef struct _pool_item_ *plPo;

typedef struct _pool_{
  size_t elsize;			/* size of each element */
  plPo free;				/* list of free elements */
  int incsize;			     /* how many to add if we need more space */
  long used;				/* How many have been allocated */
  long fcount;				/* How many in the free list */
  long alloced;				/* How many have been allocated */
  pthread_mutex_t mutex;		/* control access to the pool itself */
} pbase;

typedef struct _item_{
  unsigned long sign;		/* Pool signature */
#if DOUBLE_ALIGNMENT
  int32 pad;			/* Padding to make up to 64 bits */
#endif
} Pitem,*itemPo;

typedef struct _pool_item_{
  unsigned long sign;			/* Pool signature */
  plPo next;			/* next element in the pool */
} ppl;

#define ALIGN(count,size) (((count+size-1)/size)*(size))

static inline size_t computeElSize(size_t s)
{
#if DOUBLE_ALIGNMENT
  return ALIGN(s,sizeof(double))+sizeof(Pitem);
#else
  return s+sizeof(Pitem);
#endif
}

poolPo newPool(size_t elsize, int initial)
{
  size_t actElSize = computeElSize(elsize);
  char *alloc = calloc(initial,actElSize);
  int i = 0;
  char *p = alloc;
  poolPo pool = (poolPo)malloc(sizeof(pbase));

  if(pool==NULL || alloc==NULL)
    return NULL;

  pool->free=NULL;
  pool->elsize=actElSize;
  pool->incsize=initial;
  pool->used = 0;
  pool->fcount = 0;
  pool->alloced = 0;

  {
    pthread_mutexattr_t attr;

    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_NORMAL);

    pthread_mutex_init(&pool->mutex,&attr);
    pthread_mutexattr_destroy(&attr);
  }

  for(i=0;i<initial;i++){
    plPo el = (plPo)p;

    el->next = pool->free;
    el->sign = (unsigned long)pool;
    pool->free = el;
    pool->fcount++;
    pool->alloced++;
    p+=actElSize;
  }

  assert(pool->fcount+pool->used==pool->alloced);

  return pool;
}
  
/* Allocate an element from the pool */  
void *allocPool(poolPo pool)
{
  itemPo el = NULL;

  pthread_mutex_lock(&pool->mutex);

  assert(pool->fcount+pool->used==pool->alloced);

  if(pool->free==NULL){
    char *alloc = calloc(pool->incsize,pool->elsize);

    if(alloc!=NULL){
      int i = pool->incsize; 

      while(i--){
	plPo al = (plPo)alloc;

	al->next = pool->free;
	al->sign = (unsigned long)pool;
	pool->free = al;
	pool->fcount++;
	pool->alloced++;

	alloc += pool->elsize;
      }
    }
  }

  if(pool->free!=NULL){
    plPo p = pool->free;

#ifdef TRACEPOOL
    assert(p->sign==(unsigned long)pool);
#endif

    pool->free = p->next;
    pool->used++;
    pool->fcount--;
    p->sign = ~(unsigned long)pool;	/* set the sigature for a used entry*/
    el = (itemPo)p;
  }
  else{
    logMsg(logFile,"Memory exhausted in allocPool");
    pthread_mutex_unlock(&pool->mutex);
    exit(99);
  }

  assert(pool->fcount+pool->used==pool->alloced);

  pthread_mutex_unlock(&pool->mutex);
  return (void*)(el+1);
}

void freePool(poolPo pool, void *p)
{
  plPo el = (plPo)(((itemPo) p)-1); /* step back to the header structure */

  pthread_mutex_lock(&pool->mutex);

  assert(pool->fcount+pool->used==pool->alloced && el->sign == ~(unsigned long)pool);

  el->next = pool->free;
  el->sign = (unsigned long)pool;	/* set in the marker */
  pool->free = el;
  pool->used--;
  pool->fcount++;

  assert(pool->fcount+pool->used==pool->alloced);

  pthread_mutex_unlock(&pool->mutex);
}

void verifyPool(poolPo pool)
{
  pthread_mutex_lock(&pool->mutex);

  {
    plPo el=pool->free;

    while(el!=NULL){
      assert(el->sign==(unsigned long)pool);
      el = el->next;
    }
  }

  pthread_mutex_unlock(&pool->mutex);
}

