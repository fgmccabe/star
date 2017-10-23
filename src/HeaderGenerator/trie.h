#ifndef _TRIE_H_
#define _TRIE_H_

#include <logical.h>
#include "ooio.h"

typedef struct _trie_ *triePo;

extern triePo emptyTrie();
extern void addToTrie(char *key,void *value,triePo trie);
extern void* findInTrie(char *key,triePo trie);

typedef void (*trieProc)(char *prefix, codePoint cp, void *value,void *cl);

extern void processTrie(triePo trie, trieProc proc, void *cl, logical breadthFirst);
extern void dumpTrie(triePo trie, ioPo out);
#endif
