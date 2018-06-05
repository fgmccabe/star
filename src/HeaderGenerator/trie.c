/*
 * trie structure
 */

#include "trieP.h"
#include "pool.h"
#include <stdlib.h>
#include <string.h>
#include <iochnnl.h>
#include <ooio.h>

static poolPo triePool;

static void init() {
  if (triePool == NULL) {
    triePool = newPool(sizeof(TrieRec), 1024);
  }
}

static triePo emptyTr(char *prefix) {
  init();

  triePo emptyTr = (triePo) allocPool(triePool);

  emptyTr->prefix = prefix;
  emptyTr->follows = NULL;
  emptyTr->value = NULL;
  return emptyTr;
}

triePo emptyTrie() {
  return emptyTr("");
}

static integer charHash(void *data) {
  char ch = (char) ((long) data);
  return (integer) ch;
}

static comparison charComp(void *l, void *r) {
  char left = (char) ((long) l);
  char right = (char)((long) r);

  if (left < right)
    return smaller;
  else if (left == right)
    return same;
  else
    return bigger;
}

static void addToTr(char *key, integer pos, integer limit, void *value, triePo trie) {
  if (pos >= limit) {
    trie->value = value;
  } else {
    if (trie->follows == NULL) {
      trie->follows = NewHash(6, charHash, charComp, NULL);
    }
    integer xp = pos;
    codePoint cp = nextCodePoint(key, &pos, limit);

    triePo follows = (triePo) hashGet(trie->follows, (void *) (integer) cp);
    if (follows == NULL) {
      char *prefix = (char *) calloc((size_t) (pos + 1), sizeof(byte));
      for (int ix = 0; ix < xp; ix++)
        prefix[ix] = key[ix];
      prefix[xp] = '\0';
      follows = emptyTr(strdup(prefix));
      hashPut(trie->follows, (void *) (integer) cp, follows);
    }
    addToTr(key, pos, limit, value, follows);
  }
}

void addToTrie(char *key, void *value, triePo trie) {
  addToTr(key, 0, (integer) strlen(key), value, trie);
}

static void *trieFind(char *key, integer pos, integer limit, triePo trie) {
  if (trie == NULL)
    return NULL;
  else if (pos >= limit)
    return trie->value;
  else {
    codePoint cp = nextCodePoint(key, &pos, limit);

    triePo next = (triePo) hashGet(trie->follows, (void *) (integer) cp);
    if (next != NULL)
      return trieFind(key, pos, limit, next);
    else
      return NULL;
  }
}

void *findInTrie(char *key, triePo trie) {
  integer limit = (integer) strlen(key);
  integer pos = 0;
  return trieFind(key, pos, limit, trie);
}

typedef struct {
  void *cl;
  trieProc proc;
  logical breadthFirst;
} ClRecord, *clPo;

static retCode trieEntryProc(void *n, void *v, void *cl) {
  triePo T = (triePo) v;
  clPo P = (clPo) cl;

  P->proc(T->prefix, (codePoint)((long)n), T->value, P->cl);
  if (T->follows != NULL) {
    return ProcessTable(trieEntryProc, T->follows, cl);
  }
  return Ok;
}

static retCode procEntries(void *n, void *v, void *cl) {
  codePoint cp = (codePoint) ((long)n);
  triePo T = (triePo) v;
  clPo P = (clPo) cl;

  P->proc(T->prefix, cp, T->value, P->cl);
  return Ok;
}

static retCode procTree(void *n, void *v, void *cl) {
  triePo T = (triePo) v;
  clPo P = (clPo) cl;
  processTrie(T, P->proc, P->cl, P->breadthFirst);
  return Ok;
}

static retCode procDeeper(void *n, void *v, void *cl) {
  triePo T = (triePo) v;
  clPo P = (clPo) cl;

  if (T->follows != NULL) {
    processTrie(T, P->proc, P->cl, True);
  }
  return Ok;
}

void processTrie(triePo trie, trieProc proc, void *cl, logical breadthFirst) {
  if (trie != NULL) {
    ClRecord Cl = {cl, proc, breadthFirst};

    if (breadthFirst) {
      if (trie->follows != NULL) {
        ProcessTable(procEntries, trie->follows, &Cl);
        ProcessTable(procDeeper, trie->follows, &Cl);
      }
    } else {
      ProcessTable(trieEntryProc, trie->follows, (void *) &Cl);
    }
  }
}

static void showEntry(char *prefix, codePoint last, void *v, void *cl) {
  ioPo out = (ioPo) cl;
  outMsg(out, "%s%C\n", prefix, last);
}

void dumpTrie(triePo trie, ioPo out) {
  processTrie(trie, showEntry, out, False);
  flushOut();
}
