#ifndef _TRIE_P_H_
#define _TRIE_P_H_

#include "trie.h"
#include "hash.h"
#include "logical.h"

typedef struct _trie_ {
  char *prefix;
  void *value;
  hashPo follows;
} TrieRec;

#endif
