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
  }
}
