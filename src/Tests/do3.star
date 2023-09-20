test.do3{
  import star.
  import star.assert.

  -- Test action notation (iteration)

  doDupl:(integer,string) => string.
  doDupl(Mx,S) => valof{
    Cx = ref 0;
    Rs = ref "";
    
    while Cx! < Mx do{
      Rs := Rs! ++ S;
      Cx := Cx!+1
    };
    valis Rs!
  }

  main:()=> ().
  main() => valof{
    assert doDupl(3,"ab") == "ababab";
    valis ()
  }
}
