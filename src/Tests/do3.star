test.do3{
  import star.
  import star.script.

  -- Test action notation (iteration)

  doDupl:(integer,string) => result[(),string].
  doDupl(Mx,S) => do{
    Cx .= ref 0;
    Rs .= ref "";
    
    while Cx! < Mx do{
      Rs := Rs! ++ S;
      Cx := Cx!+1
    };
    valis Rs!
  }

  main:()=> ().
  main() => valof{
    try{
      assert valof doDupl(3,"ab") == "ababab"
    } catch {
      _ => logMsg("huh?")
    };
    valis ()
  }
}
