//
// Created by Francis McCabe on 12/11/25.
//

#include "treeP.h"
#include "pool.h"

static poolPo nodePool = Null;
static poolPo treePool = Null;

static void initTree() {
  if (nodePool == Null) {
    nodePool = newPool(sizeof(nodeRec), 16);
    treePool = newPool(sizeof(TreeTableRec), 16);
  }
}

/* Create a new table */
treePo newTree(compFun cmp, destFun dest) {
  initTree();

  treePo tr = (treePo) allocPool(treePool);

  tr->tree = Null;
  tr->compare = cmp;
  tr->destroy = dest;

  initRecursiveMutex(&tr->mutex);

  return tr;
}

static retCode delnodes(treePo tr, nodePo nd) {
  retCode ret = Ok;
  if (nd->left != Null)
    ret = delnodes(tr, nd->left);
  if (ret == Ok && nd->right != Null)
    ret = delnodes(tr, nd->right);

  if (ret == Ok && tr->destroy != Null)
    ret = tr->destroy(nd->nme, nd->r);

  freePool(nodePool, nd);
  return ret;
}

retCode eraseTree(treePo hp) {
  retCode ret = Ok;

  pthread_mutex_lock(&hp->mutex);
  if (hp->tree != Null) {
    ret = delnodes(hp, hp->tree);
  }

  pthread_mutex_unlock(&hp->mutex);
  pthread_mutex_destroy(&hp->mutex);
  freePool(treePool, hp);

  return ret;
}

/* Search the tree */
static void *nodeSearch(treePo tr, nodePo nd, void *name) {
  switch (tr->compare(name, nd->nme)) {
    case smaller: /* One item is smaller than another */
      if (nd->left != Null)
        return nodeSearch(tr, nd->left, name);
      else
        return Null;
    case same: /* Two items are the same */
      return nd->r;
    case bigger:
      if (nd->right != Null)
        return nodeSearch(tr, nd->right, name);
      else
        return Null;
    case different:
      syserr("invalid node compare");
      return Null;
  }
}

void *treeGet(treePo tr, void *name) {
  if (tr->tree != Null) {
    pthread_mutex_lock(&tr->mutex);
    void *r = nodeSearch(tr, tr->tree, name);
    pthread_mutex_unlock(&tr->mutex);
    return r;
  }
  return Null;
}

/* Install in the tree */

static nodePo installNode(treePo tr, nodePo nd, void *name, void *r) {
  if (nd != Null) {
    switch (tr->compare(name, nd->nme)) {
      case smaller:
        nd->left = installNode(tr, nd->left, name, r);
        break;
      case same:
        nd->r = r;
        break;
      case bigger:
        nd->right = installNode(tr, nd->right, name, r);
        break;
      case different:
        syserr("invalid conparison result");
    }
    return nd;
  } else {
    nd = (nodePo) allocPool(nodePool);
    nd->nme = name;
    nd->r = r;
    return nd;
  }
}

retCode treePut(treePo tr, void *name, void *r) {
  pthread_mutex_lock(&tr->mutex);

  tr->tree = installNode(tr, tr->tree, name, r);
  pthread_mutex_unlock(&tr->mutex);
  return Ok;
}

nodePo mergeNodes(treePo tr, nodePo left, nodePo right) {
  if (left == Null)
    return right;
  else if (right == Null)
    return left;
  else {
    switch (tr->compare(left->nme, right->nme)) {
      case smaller:
        left->right = mergeNodes(tr, left->right, right);
        return left;
      case bigger:
        right->left = mergeNodes(tr, left, right->left);
        return right;
      default:
        syserr("invalid conparison result");
        return Null;
    }
  }
}

/* remove an entry from the tree */
nodePo removeNode(treePo tr, nodePo nd, void *name) {
  if (nd != Null) {
    switch (tr->compare(name, nd->nme)) {
      case smaller:
        nd->left = removeNode(tr, nd->left, name);
        return nd;
      case same: {
        if (nd->left != Null) {
          if (nd->right != Null) {
            nodePo merged = mergeNodes(tr, nd->left, nd->right);
            freePool(nodePool, nd);
            return merged;
            // Both non-null
          } else {
            freePool(nodePool, nd);
            return nd->left;
          }
        } else {
          freePool(nodePool, nd);
          return nd->right;
        }
      }
      case bigger:
        nd->right = removeNode(tr, nd->right, name);
        return nd;

      case different:
        syserr("invalid conparison result");
        return nd;
    }
  }
  return Null;
}

retCode treeRemove(treePo tr, void *name) {
  pthread_mutex_lock(&tr->mutex);
  tr->tree = removeNode(tr, tr->tree, name);
  pthread_mutex_unlock(&tr->mutex);
  return Ok;
}

/* Process the whole tree */
static retCode processNode(treePo tr, nodePo nd, procFun pr, void *cl) {
  if (nd != Null) {
    retCode ret = processNode(tr, nd->left, pr, cl);
    if (ret == Ok)
      ret = pr(nd->nme, nd->r, cl);
    if (ret == Ok)
      ret = processNode(tr, nd->right, pr, cl);
    return ret;
  }
  return Ok;
}

retCode processTree(procFun pr, treePo tree, void *c) {
  pthread_mutex_lock(&tree->mutex);

  retCode ret = processNode(tree, tree->tree, pr, c);
  pthread_mutex_unlock(&tree->mutex);
  return ret;
}

integer nodeSize(treePo t, nodePo nd) {
  if (nd != Null)
    return nodeSize(t, nd->left) + nodeSize(t, nd->right) + 1;
  else
    return 0;
}

integer treeSize(treePo tr) {
  pthread_mutex_lock(&tr->mutex);
  integer size = nodeSize(tr, tr->tree);
  pthread_mutex_unlock(&tr->mutex);
  return size;
}
