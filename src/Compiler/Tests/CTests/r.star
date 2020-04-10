test.r{
  import star.
  import star.script.

  tv ::= tv{bind : ref option[tp]. }.

  tp ::= nomnal(string).

  implementation display[tp] => {.
    disp(nomnal(Nm))=>ss(Nm).
  .}

  implementation display[tv] => {.
    disp(R) => ssSeq([ss("tv{ "),ss("bind ="),disp(R.bind!!),ss("}")])
  .}

  setB:(tv,tp)=>action[(),()].
  setB(t,T) => do{
    t.bind := some(T)
  }

  main:()=>action[(),()].
  main()=>do{
    foo .= tv{bind := .none};
    foo.bind := some(nomnal("foo"));
    show foo;
    bar .= tv{. bind := .none.};
    show bar
  }
}
