test.phone{
  import star.
  import star.trie.
  import star.script.

  -- Implement phone encoding algorithm

  digitMap:map[integer,cons[integer]].
  digitMap = [0 -> [0ce],
    1 -> [0cj, 0cn, 0cq],
    2 -> [0cr, 0cw, 0cx],
    3 -> [0cd, 0cs, 0cy],
    4 -> [0cf, 0ct],
    5 -> [0ca, 0cm],
    6 -> [0cc, 0ci, 0cv],
    7 -> [0cb, 0ck, 0cu],
    8 -> [0cl, 0co, 0cp],
    9 -> [0cg, 0ch, 0cz]].

  parseWord:(string,trie[integer,string]) => trie[integer,string].
  parseWord(Word,Tr) =>
    insertInTrie(sanitize(Word),Word,Tr).

  parseDict:(cons[string],trie[integer,string]) =>trie[integer,string].
  parseDict(Dict,Tr) => foldRight(parseWord,Tr,Dict).

  sanitize(Word) => (((Word::cons[integer])^/isLetter)//lowerCase).

  lowerCase(C) where _isLuChar(C) => C+32.
  lowerCase(X) default => X.

/*  encode:(cons[integer],trie[integer,string]) => cons[cons[string]].
  encode([],_) => [].
  encode([C,..W],Tr) =>
  */
  main:()=>action[(),()].
  main()=>action{
    show sanitize("TrusC");
    valis ()
  }
  
}
