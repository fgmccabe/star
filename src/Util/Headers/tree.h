//
// Created by Francis McCabe on 12/11/25.
//

#ifndef STAR_TREE_H
#define STAR_TREE_H

#include "retcode.h"
#include "integer.h"
#include "hash.h" // Pick up some definitions from hash tables

/* Hash table interface */
typedef struct bintree_ *treePo;

typedef comparison (*compFun)(void *, void *); /* Comparison function */
typedef retCode (*destFun)(void *, void *); /* Destroy function */
typedef retCode (*procFun)(void *n, void *r, void *c); /* Processing func */

/* Build a new tree */
treePo newTree(compFun cmp, destFun dest);
retCode eraseTree(treePo hp);
retCode processTree(procFun pr, treePo tree, void *c);

// Use these instead
retCode treePut(treePo tree,void *name, void *r); // install a new entry
void *treeGet(treePo tree,void *name); // search for an entry
retCode treeRemove(treePo tree,void *name); // remove an entry from the hash tabl
integer treeSize(treePo tree);

#endif //STAR_TREE_H
