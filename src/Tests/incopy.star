test.incopy{
  import star.
  import star.assert.
  import star.io.
  import star.inchnnl.

  readFl:(string) => option[cons[char]].
  readFl(src) => valof{
    try{
      Ch = inChannel(src);

      valis .some(readAll(Ch))
    } catch errorCode in {
      _ => valis .none
    }
  }

  readAll:all x ~~ stream[x->>char] |: (x) => cons[char].
  readAll([]) => [].
  readAll([H,..T]) => [H,..readAll(T)].

  _main:(cons[string])=>().
  _main([S,.._]) => main(S).
  _main([]) => main("incopy.star").

  main:(string)=>().
  main(S) => valof{
    Text = readFl(S);

    logMsg("read of #(S) is $(Text)");
    valis ()
  }
}
  
