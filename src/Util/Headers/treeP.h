//
// Created by Francis McCabe on 12/11/25.
//

#ifndef STAR_TREEP_H
#define STAR_TREEP_H

#include "tree.h"
#include "hash.h"
#include "formio.h"

/* The structure of a binary tree node ... */
typedef struct node_ *nodePo;

typedef struct node_ {
  void *nme; /* The symbol in the hash table */
  void *r; /* 'value' of the hashed record */
  nodePo left; // Sub-tree that has a smaller key
  nodePo right; // Sub-tree that has a higher key value
} nodeRec;

typedef struct bintree_ {
  nodePo tree; // The node elements in the tree
  compFun compare; /* The comparison function */
  destFun destroy; /* Entry destruction function */
  pthread_mutex_t mutex; /* Mutex associated with table */
} TreeTableRec;

#endif //STAR_TREEP_H
