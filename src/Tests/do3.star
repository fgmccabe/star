test.do3{
  import star.
  import star.script.

  -- Test action notation (iteration)

  doDupl:(integer,string) => action[(),string].
  doDupl(Mx,S) => action{
    Cx .= ref 0;
    Rs .= ref "";
    
    while Cx! < Mx do{
      Rs := Rs! ++ S;
      Cx := Cx!+1
    };
    valis Rs!
  }

  main:()=>action[(),()].
  main() => action{
    assert valof doDupl(3,"ab") == "ababab"
  }
}
