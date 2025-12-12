#ifndef _TRIE_P_H_
#define _TRIE_P_H_

#include "stringTrie.h"
#include "tree.h"
#include "logical.h"

typedef struct stringTrie_ {
  char *prefix;
  void *value;
  treePo follows;
} StringTrieRec;

#endif
