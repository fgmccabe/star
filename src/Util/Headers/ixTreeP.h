/*
  Private Interface to indextree
  Applicative tree with performance near hash tree

  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _U_IXTREEP_H
#define _U_IXTREEP_H

#include "ixTree.h"
#include "objectP.h"
#include "list.h"
#include "pair.h"

#include <stdarg.h>

typedef struct _ix_tree_leaf_object_ *leafPo;
typedef struct _ix_tree_node_object_ *nodePo;


typedef struct {
  objectPo (*find)(treePo tree, objectPo key, integer hash);
  treePo (*delete)(treePo tree, objectPo key, integer hash);
  treePo (*merge)(treePo t1, treePo t2);
  logical (*isEmpty)(treePo t);
  int32 (*size)(treePo t);
  void *(*fold)(treePo t, ixFolder f, void *init);
} IxTreeClassPartRec;

typedef struct {
  treePo l1;                          // A node has (up to) four child trees
  treePo l2;
  treePo r1;
  treePo r2;
} IxTreeNodeObjectPartRec;

typedef struct {
  listPo leafs;                      // All the entries with this hash value
} IxLeafNodeObjectPartRec;

typedef struct _ixtree_leaf_class {
  ObjectClassRec objectPart;
  IxTreeClassPartRec treePart;
} IxTreeClassRec, *treeClassPo;

extern IxTreeClassRec TreeLeafClass;
/* the standard pointer to an IxTree class record */
extern IxTreeClassRec TreeNodeClass;

typedef struct _tree_part_ {          /* The generic part of an index tree */
  integer mask;                        /* The tree mask */
  int16 masklen;                      /* The active length of the mask */
} IxTreePart;

typedef struct _ix_tree_ {
  ObjectRec object;
  IxTreePart tree;
} IxTreeObject;

typedef struct _ix_tree_leaf_object_ {
  ObjectRec object;
  IxTreePart tree;                    /* Abstract tree part of ixTree */
  IxLeafNodeObjectPartRec leaf;
} IxLeafObject;

typedef struct _ix_tree_node_object_ {
  ObjectRec object;                   /* object level of the tree node structure */
  IxTreePart tree;                    /* Abstract tree part of ixTree */
  IxTreeNodeObjectPartRec node;
} IxNodeObject;

extern classPo leafClass, nodeClass;

#ifdef VERIFY_OBJECT
#define O_LEAF(c) ((leafPo)(checkCast((c),leafClass)))
#define O_NODE(c) ((nodePo)(checkCast((c),nodeClass)))
#else
#define O_LEAF(c) ((leafPo)(c))
#define O_NODE(c) ((nodePo)(c))
#endif

#endif //_U_IXTREEP_H
