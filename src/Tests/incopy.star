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
    } catch {
      _ do valis .none
    }
  }

  readAll:all x ~~ stream[x->>char] |= (x) => cons[char].
  readAll([]) => [].
  readAll([H,..T]) => [H,..readAll(T)].

  _main:(cons[string])=>integer.
  _main([S,.._]) => valof{ main(S); valis 0}.
  _main([]) => valof{ main("incopy.star"); valis 0}

  main:(string){}.
  main(S){
    Text = readFl(S);

    showMsg("read of #(S) is $(Text)");
  }
}
  
