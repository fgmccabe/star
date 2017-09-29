/*
  Index tree
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <assert.h>
#include <ixTreeP.h>
#include "ixTreeP.h"
#include "utils.h"

// Implement the tree leaf class structure

static void inheritTree(classPo class, classPo request);
static void initLeafClass(classPo class, classPo request);
static void eraseLeaf(objectPo o);
static void eraseNode(objectPo o);
static void leafInit(objectPo o, va_list *args);
static void nodeInit(objectPo o, va_list *args);
static integer leafHash(objectPo o);
static integer nodeHash(objectPo o);
static logical leafEquality(objectPo o1, objectPo o2);
static logical nodeEquality(objectPo o1, objectPo o2);

static objectPo findInTree(treePo tree, objectPo key, integer hash);
static objectPo findInLeaf(treePo tree, objectPo key, integer hash);
static objectPo findInNode(treePo tree, objectPo key, integer hash);

static treePo deleteFromTree(treePo tree, objectPo key, integer hash);
static treePo deleteFromLeaf(treePo tree, objectPo key, integer hash);
static treePo deleteFromNode(treePo tree, objectPo key, integer hash);

static treePo mergeTree(treePo t1, treePo t2);
static treePo mergeWithLeaf(treePo t1, treePo t2);
static treePo mergeNode(treePo t1, treePo t2);

static logical leafIsEmpty(treePo t);
static logical nodeIsEmpty(treePo t);

int32 leafSize(treePo t);
int32 nodeSize(treePo t);

void *leafFold(treePo t, ixFolder f, void *init);
void *nodeFold(treePo t, ixFolder f, void *init);

static treePo newNode(int masklen, integer mask, treePo l1, treePo l2, treePo r1, treePo r2);

static treePo emptyTree;

IxTreeClassRec TreeLeafClass = {
  {
    (classPo) &ObjectClass,                 /* parent class is object */
    "leafTree",                             /* this is the tree leaf class */
    inheritTree,                            /* inherit class init */
    initLeafClass,                          /* tree leaf class init */
    O_INHERIT_DEF,                          /* Leaf object element creation */
    O_INHERIT_DEF,                          /* Leaf objectdestruction */
    eraseLeaf,                              /* erasure */
    leafInit,                               /* initialization of a leaf tree object */
    sizeof(IxLeafObject),                   /* size of a leaf object */
    leafHash,                               // Hashcode for leaf
    leafEquality,                           // Equality for leaf nodes
    NULL,                                   /* pool of leaf values */
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    findInLeaf,                             // Find elemements in leaf
    deleteFromLeaf,                         // Delete from leaf
    mergeWithLeaf,                          // Merge trees
    leafIsEmpty,                            // isEmpty test
    leafSize,                               // size of leaf tree
    leafFold                                // Fold across leafs
  }
};

classPo leafClass = (classPo) &TreeLeafClass;

void inheritTree(classPo class, classPo request) {
  IxTreeClassRec *req = (IxTreeClassRec *) request;
  IxTreeClassRec *template = (IxTreeClassRec *) class;

  logical done = False;

  while (!done) {
    done = True;

    if (req->treePart.find == O_INHERIT_DEF) {
      if (template->treePart.find != O_INHERIT_DEF)
        req->treePart.find = template->treePart.find;
      else
        done = False;
    }

    if (req->treePart.delete == O_INHERIT_DEF) {
      if (template->treePart.delete != O_INHERIT_DEF)
        req->treePart.delete = template->treePart.delete;
      else
        done = False;
    }

    if (req->treePart.merge == O_INHERIT_DEF) {
      if (template->treePart.merge != O_INHERIT_DEF)
        req->treePart.merge = template->treePart.merge;
      else
        done = False;
    }

    if (req->treePart.isEmpty == O_INHERIT_DEF) {
      if (template->treePart.isEmpty != O_INHERIT_DEF)
        req->treePart.isEmpty = template->treePart.isEmpty;
      else
        done = False;
    }

    if (req->treePart.size == O_INHERIT_DEF) {
      if (template->treePart.size != O_INHERIT_DEF)
        req->treePart.size = template->treePart.size;
      else
        done = False;
    }

    if (req->treePart.fold == O_INHERIT_DEF) {
      if (template->treePart.fold != O_INHERIT_DEF)
        req->treePart.fold = template->treePart.fold;
      else
        done = False;
    }

    template = (IxTreeClassRec *) (template->objectPart.parent);
  }
}

static pthread_once_t ioOnce = PTHREAD_ONCE_INIT;

static void initLeafTree(void) {
  initRecursiveMutex(&leafClass->mutex);
  emptyTree = (treePo) newObject(leafClass, nilList, 0);
}

static void initLeafClass(classPo class, classPo request) {
  pthread_once(&ioOnce, initLeafTree);
}

static void eraseLeaf(objectPo o) {
  leafPo leaf = O_LEAF(o);

  decReference(O_OBJECT(leaf->leaf.leafs));
}

static void leafInit(objectPo o, va_list *args) {
  leafPo l = O_LEAF(o);
  l->leaf.leafs = cons(O_OBJECT(pair(va_arg(*args, objectPo), va_arg(*args, objectPo))), nilList);
  l->tree.mask = va_arg(*args, integer);
}

static leafPo newLeaf(objectPo key, objectPo val) {
  return (leafPo) newObject(leafClass, key, val, hashCode(key));
}

static integer leafHash(objectPo o) {
  return hashCode(O_OBJECT(O_LEAF(o)->leaf.leafs));
}

static logical leafEquality(objectPo o1, objectPo o2) {
  leafPo l1 = O_LEAF(o1);
  leafPo l2 = O_LEAF(o2);

  if (ixSize((treePo) l1) == ixSize((treePo) l2)) {
    for (listPo ll1 = l1->leaf.leafs; ll1 != nilList; ll1 = tail(ll1)) {
      for (listPo ll2 = l2->leaf.leafs; ll2 != nilList; ll2 = tail(ll2)) {
        if (equals(head(ll1), head(ll2)))
          goto leafLoop;
      }
      return False;
      leafLoop:
      continue;
    }
    return True;
  } else
    return False;
}

static objectPo findInLeaf(treePo tree, objectPo key, integer hash) {
  leafPo l = O_LEAF(tree);
  for (listPo ll = l->leaf.leafs; ll != nilList; ll = tail(ll)) {
    pairPo p = O_PAIR(head(ll));
    if (equals(lhs(p), key))
      return rhs(p);
  }
  return NULL;
}

static logical testLeafs(objectPo o, void *cl) {
  pairPo p = O_PAIR(o);
  objectPo k = O_OBJECT(cl);

  return equals(k, lhs(p));
}

static treePo deleteFromLeaf(treePo tree, objectPo key, integer hash) {
  leafPo l = O_LEAF(tree);
  listPo d = filter(l->leaf.leafs, testLeafs, key);

  return (treePo) newObject(leafClass, d, hash);
}

static int16 common2WayMaskLen(integer h1, integer h2) {
  int16 C = 64;
  while (h1 != h2 && C > 0) {
    h1 = h1 >> 2;
    h2 = h2 >> 2;
    C -= 2;
  }
  return C;
}

static int16 nth4way(integer mask, int16 pos) {
  mask = mask >> (62 - pos);
  return (int16) (mask & 3);
}

static integer maskPrefix(integer mask, int16 len) {
  if (len == 0)
    return 0;
  else {
    int16 cml = (int16) 62 - len;
    integer msb = ((integer) -1) >> cml;
    integer lhsMask = msb << cml;
    return lhsMask & mask;
  }
}

static treePo mergeWithLeaf(treePo t1, treePo t2) {
  if (objectHasClass(O_OBJECT(t2), leafClass)) {
    leafPo l1 = O_LEAF(t1);
    leafPo l2 = O_LEAF(t2);

    if (l1->tree.mask == l2->tree.mask) {
      listPo nl = l2->leaf.leafs;
      for (listPo ll1 = l1->leaf.leafs; ll1 != nilList; ll1 = tail(ll1)) {
        for (listPo ll2 = l2->leaf.leafs; ll2 != nilList; ll2 = tail(ll2)) {
          if (equals(lhs(head(ll1)), lhs(head(ll2))))
            goto nextLL1;
        }
        nl = cons(head(ll1), nl);
        nextLL1:;
      }
      return (treePo) newObject(leafClass, nl, l1->tree.mask);
    } else if (leafIsEmpty(t2))
      return t1;
    else if (leafIsEmpty(t1))
      return t2;
    else {
      int16 cml = common2WayMaskLen(l1->tree.mask, l2->tree.mask);
      integer cm = maskPrefix(l1->tree.mask, cml);

      treePo L1 = emptyTree;
      treePo L2 = emptyTree;
      treePo R1 = emptyTree;
      treePo R2 = emptyTree;

      switch (nth4way(l1->tree.mask, cml)) {
        case 0:
          L1 = t1;
          break;
        case 1:
          L2 = t1;
          break;
        case 2:
          R1 = t1;
          break;
        case 3:
          R2 = t1;
        default:
          syserr("illegal state");
      }

      switch (nth4way(l2->tree.mask, cml)) {
        case 0:
          assert(L1 == emptyTree);
          L1 = t2;
          break;
        case 1:
          assert(L2 == emptyTree);
          L2 = t2;
          break;
        case 2:
          assert(R1 == emptyTree);
          R1 = t2;
          break;
        case 3:
          assert(R2 == emptyTree);
          R2 = t2;
        default:
          syserr("illegal state");
      }

      return newNode(cml, cm, L1, L2, R1, R2);
    }
  } else
    return mergeNode(t2, t1);
}

static logical leafIsEmpty(treePo t) {
  return (logical) (O_LEAF(t)->leaf.leafs == nilList);
}

int32 leafSize(treePo t) {
  return (int32) listCount(O_LEAF(t)->leaf.leafs);
}

void *leafFold(treePo t, ixFolder f, void *state) {
  listPo l = O_LEAF(t)->leaf.leafs;
  while (l != nilList) {
    pairPo h = O_PAIR(head(l));
    state = f(lhs(h), rhs(h), state);
    l = tail(l);
  }
  return state;
}

IxTreeClassRec TreeNodeClass = {
  {
    (classPo) &ObjectClass,                 /* parent class is object */
    "nodeTree",                             /* this is the tree leaf class */
    inheritTree,                            /* inherit class init */
    O_INHERIT_DEF,                          /* tree leaf create */
    O_INHERIT_DEF,                          /* Leaf object element creation */
    O_INHERIT_DEF,                          /* Leaf objectdestruction */
    eraseNode,                              /* erasure */
    nodeInit,                               /* initialization of a node tree object */
    sizeof(IxNodeObject),                   /* size of a leaf object */
    nodeHash,                               // Hashcode for node
    nodeEquality,                           // Equality for nodes
    NULL,                                   /* pool of leaf values */
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {
    findInNode,                             // Find elemements in node
    deleteFromNode,                         // Delete from node
    mergeNode,                              // Merge node
    nodeIsEmpty,                            // isEmpty test
    nodeSize,                               // size of node tree
    nodeFold                                // Fold across node
  }
};

classPo nodeClass = (classPo) &TreeNodeClass;

static void eraseNode(objectPo o) {
  nodePo node = O_NODE(o);

  decReference(O_OBJECT(node->node.l1));
  decReference(O_OBJECT(node->node.l2));
  decReference(O_OBJECT(node->node.r1));
  decReference(O_OBJECT(node->node.r1));
}

static void nodeInit(objectPo o, va_list *args) {
  nodePo node = O_NODE(o);

  node->tree.masklen = (int16) va_arg(*args, int);
  node->tree.mask = va_arg(*args, integer);
  node->node.l1 = va_arg(*args, treePo);
  node->node.l2 = va_arg(*args, treePo);
  node->node.r1 = va_arg(*args, treePo);
  node->node.r2 = va_arg(*args, treePo);
}

static treePo newNode(int masklen, integer mask, treePo l1, treePo l2, treePo r1, treePo r2) {
  return (treePo) newObject(nodeClass, masklen, mask, l1, l2, r1, r2);
}

static integer nodeHash(objectPo o) {
  nodePo node = O_NODE(o);
  return
    37 * (37 * (37 * hashCode(O_OBJECT(node->node.l1)) + hashCode(O_OBJECT(node->node.l2))) +
          hashCode(O_OBJECT(node->node.r1))) + hashCode(O_OBJECT(node->node.r2));
}

static logical nodeEquality(objectPo o1, objectPo o2) {
  if (objectHasClass(o2, nodeClass)) {
    nodePo n1 = O_NODE(o1);
    nodePo n2 = O_NODE(o2);
    return (logical) (equals(O_OBJECT(n1->node.l1), O_OBJECT(n2->node.l1)) &&
                      equals(O_OBJECT(n1->node.l2), O_OBJECT(n2->node.l2)) &&
                      equals(O_OBJECT(n1->node.r1), O_OBJECT(n2->node.r1)) &&
                      equals(O_OBJECT(n1->node.r2), O_OBJECT(n2->node.r2)));
  }
  else
    return False;
}

static objectPo findInNode(treePo tree, objectPo key, integer hash) {
  nodePo node = O_NODE(tree);
  integer common = maskPrefix(hash, tree->tree.masklen);

  if (common == tree->tree.mask) {
    switch (nth4way(hash, tree->tree.masklen)) {
      case 0:
        return findInTree((treePo) (node->node.l1), key, hash);
      case 1:
        return findInTree((treePo) (node->node.l2), key, hash);
      case 2:
        return findInTree((treePo) (node->node.r1), key, hash);
      case 3:
        return findInTree((treePo) (node->node.r2), key, hash);
      default:
        syserr("bad case");
    }
  }
  return NULL;
}

static objectPo findInTree(treePo tree, objectPo key, integer hash) {
  IxTreeClassRec *treeClass = (IxTreeClassRec *) tree->object.class;
  return treeClass->treePart.find(tree, key, hash);
}

objectPo ixFind(treePo tree, objectPo key) {
  return findInTree(tree, key, hashCode(key));
}

static treePo deleteFromNode(treePo tree, objectPo key, integer hash) {
  nodePo n = O_NODE(tree);
  integer mask = n->tree.mask;
  int16 masklen = n->tree.masklen;

  integer cm = maskPrefix(hash, masklen);

  if (cm == mask) {
    treePo l1 = n->node.l1;
    treePo l2 = n->node.l2;
    treePo r1 = n->node.r1;
    treePo r2 = n->node.r2;

    switch (nth4way(hash, masklen)) {
      case 0:
        l1 = deleteFromTree(l1, key, hash);
        break;
      case 1:
        l2 = deleteFromTree(l2, key, hash);
        break;
      case 2:
        r1 = deleteFromTree(r1, key, hash);
        break;
      case 3:
        r2 = deleteFromTree(r2, key, hash);
        break;
      default:
        syserr("bad case");
    }

    treePo nt = l1;
    if (ixIsEmpty(nt))
      nt = l2;
    else if (!ixIsEmpty(l2))
      return newNode(masklen, mask, l1, l2, r1, r2);

    if (ixIsEmpty(nt))
      nt = r1;
    else if (!ixIsEmpty(r1))
      return newNode(masklen, mask, l1, l2, r1, r2);

    if (ixIsEmpty(nt))
      return r2;
    else
      return newNode(masklen, mask, l1, l2, r1, r2);
  } else
    return tree;
}

static treePo mergeNode(treePo t1, treePo t2) {
  nodePo n1 = O_NODE(t1);

  integer m1 = t1->tree.mask;
  integer m2 = t2->tree.mask;

  int16 ml1 = t1->tree.masklen;
  int16 ml2 = t2->tree.masklen;

  int16 cml = (int16) min(ml1, common2WayMaskLen(m1, m2));
  integer cm = maskPrefix(m1, cml);

  if (cml < ml1) {
    switch (nth4way(m2, cml)) {
      case 0:
        switch (nth4way(m1, cml)) {
          case 1:
            return newNode(cml, cm, t2, t1, emptyTree, emptyTree);
          case 2:
            return newNode(cml, cm, t2, emptyTree, t1, emptyTree);
          case 3:
            return newNode(cml, cm, t2, emptyTree, emptyTree, t1);
          default:
            syserr("bad case");
        }
      case 1:
        switch (nth4way(m1, cml)) {
          case 0:
            return newNode(cml, cm, t1, t2, emptyTree, emptyTree);
          case 2:
            return newNode(cml, cm, emptyTree, t2, t1, emptyTree);
          case 3:
            return newNode(cml, cm, emptyTree, t2, emptyTree, t1);
          default:
            syserr("bad case");
        }
      case 2:
        switch (nth4way(m1, cml)) {
          case 0:
            return newNode(cml, cm, t1, emptyTree, t2, emptyTree);
          case 1:
            return newNode(cml, cm, emptyTree, t1, t2, emptyTree);
          case 3:
            return newNode(cml, cm, emptyTree, emptyTree, t2, t1);
          default:
            syserr("bad case");
        }
      case 3:
        switch (nth4way(m1, cml)) {
          case 0:
            return newNode(cml, cm, t1, emptyTree, emptyTree, t2);
          case 1:
            return newNode(cml, cm, emptyTree, t1, emptyTree, t2);
          case 2:
            return newNode(cml, cm, emptyTree, emptyTree, t1, t2);
          default:
            syserr("bad case");
        }
      default:
        syserr("bad case");
    }
  } else if (objectHasClass(O_OBJECT(t2), leafClass)) {
    switch (nth4way(m2, cml)) {
      case 0:
        return newNode(cml, cm, mergeTree(n1->node.l1, t2), n1->node.l2, n1->node.r1, n1->node.r2);
      case 1:
        return newNode(cml, cm, n1->node.l1, mergeTree(n1->node.l2, t2), n1->node.r1, n1->node.r2);
      case 2:
        return newNode(cml, cm, n1->node.l1, n1->node.l2, mergeTree(n1->node.r1, t2), n1->node.r2);
      case 3:
        return newNode(cml, cm, n1->node.l1, n1->node.l2, n1->node.r1, mergeTree(n1->node.r2, t2));
      default:
        syserr("bad case");
    }
  } else {
    assert(objectHasClass(O_OBJECT(t2), nodeClass));
    nodePo n2 = O_NODE(t2);

    return newNode(cml, cm, mergeTree(n1->node.l1, n2->node.l1), mergeTree(n1->node.l2, n2->node.l2),
                   mergeTree(n1->node.r1, n2->node.r1), mergeTree(n1->node.r2, n2->node.r2));
  }
  return NULL; // never gets here
}

static logical nodeIsEmpty(treePo t) {
  return False;
}

int32 nodeSize(treePo t) {
  nodePo n = O_NODE(t);
  return (int32) (ixSize(n->node.l1) + ixSize(n->node.l2) + ixSize(n->node.r1) + ixSize(n->node.r2));
}

void *nodeFold(treePo t, ixFolder f, void *state) {
  nodePo n = O_NODE(t);

  state = ixFold(n->node.l1, f, state);
  state = ixFold(n->node.l2, f, state);
  state = ixFold(n->node.r1, f, state);
  return ixFold(n->node.r2, f, state);
}

static treePo deleteFromTree(treePo tree, objectPo key, integer hash) {
  treeClassPo class = (treeClassPo) (tree->object.class);
  return class->treePart.delete(tree, key, hash);
}

static treePo mergeTree(treePo t1, treePo t2) {
  treeClassPo class = (treeClassPo) (t1->object.class);
  return class->treePart.merge(t1, t2);
}
