test.st{
  import star.

  hs:(string) => option[integer].
  hs([H,..T]) => some(H).
  hs(_) default => none.

  show disp(hs("fred"))::string.
}
