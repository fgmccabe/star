test.phone{
  import star.
  import star.trie.
  import star.assert.

  -- Implement phone encoding algorithm

  digitMap:map[integer,cons[char]].
  digitMap = {0 -> [`e`],
    1 -> [`j`, `n`, `q`],
    2 -> [`r`, `w`, `x`],
    3 -> [`d`, `s`, `y`],
    4 -> [`f`, `t`],
    5 -> [`a`, `m`],
    6 -> [`c`, `i`, `v`],
    7 -> [`b`, `k`, `u`],
    8 -> [`l`, `o`, `p`],
    9 -> [`g`, `h`, `z`]}.

  parseWord:(string,trie[char,string]) => trie[char,string].
  parseWord(Word,Tr) =>
    insertInTrie(sanitize(Word),Word,Tr).

  parseDict:(cons[string],trie[char,string]) =>trie[char,string].
  parseDict(Dict,Tr) => foldRight(parseWord,Tr,Dict).

  sanitize(Word) => (((Word::cons[char])^/isLetter)//lowerCase).

  lowerCase(C) where _isLuChar(C) => _char(_codePoint(C)+32).
  lowerCase(X) default => X.

/*  encode:(cons[integer],trie[integer,string]) => cons[cons[string]].
  encode([],_) => [].
  encode([C,..W],Tr) =>
  */
  main:()=>().
  main()=>valof{
    show sanitize("TrusC");
    valis ()
  }
  
}
