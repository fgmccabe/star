test.st{
  import star.
  import star.script.

  hs:(string) => option[char].
  hs([H,..T]) => some(H).
  hs(_) default => .none.

  main:() => action[(),()].
  main() => action{
    show hs("fred");

    assert [`h`,`o`,`l`,`l`,`o`] == "hollo"
  }
}
