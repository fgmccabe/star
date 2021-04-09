#ifndef _TRIE_H_
#define _TRIE_H_

#include <logical.h>
#include "ooio.h"

typedef struct stringTrie_ *stringTriePo;

extern stringTriePo emptyStringTrie();
extern void addToStringTrie(char *key, void *value, stringTriePo trie);
extern void* findInStringTrie(char *key, stringTriePo trie);

typedef void (*stringTrieProc)(char *prefix, codePoint cp, void *value, void *cl);

extern void processStringTrie(stringTriePo trie, stringTrieProc proc, void *cl, logical breadthFirst);
extern void dumpStringTrie(stringTriePo trie, ioPo out);
#endif
