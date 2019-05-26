test.th{
  import star.
  import star.thunk.

  X := 0.

  Th = thunk(()=> valof do {
      logMsg("called Th");
      X := X!+2;
      valis 34}).

  assert Th()==34.
  assert Th()==34.
  assert (X!)==2.
}
