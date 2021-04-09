#ifndef _TRIE_P_H_
#define _TRIE_P_H_

#include "stringTrie.h"
#include "hash.h"
#include "logical.h"

typedef struct stringTrie_ {
  char *prefix;
  void *value;
  hashPo follows;
} StringTrieRec;

#endif
