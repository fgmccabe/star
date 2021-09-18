test.st{
  import star.
  import star.script.

  hs:(string) => option[integer].
  hs([H,..T]) => some(H).
  hs(_) default => .none.

  main:() => action[(),()].
  main() => action{
    show hs("fred")
  }
}
