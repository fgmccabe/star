test.st{
  import star.
  import star.assert.

  hs:(string) => option[char].
  hs([H,..T]) => .some(H).
  hs(_) default => .none.

  main:(){}.
  main(){
    show hs("fred");

    assert [`h`,`o`,`l`,`l`,`o`] == "hollo";

    show levenshtein("kitten","sitting");
    show levenshtein("Sunday","Saturday");
    assert levenshtein("hello","hello")==0;
    assert levenshtein("hello","helo")==1;
    assert levenshtein("Sunday","Saturday")==3;
    assert levenshtein("kitten","sitting")==3;
  }
}
