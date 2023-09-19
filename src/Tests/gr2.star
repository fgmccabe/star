test.gr2{
  import star.
  import star.script.

  -- Test grammar sequences

  ident >> [F,..S]::string --> letter >> F, letterDigit* >>S.

  letter >> L --> [L], {isLetter(L)}.

  letterDigit:(cons[char]) => option[(char,cons[char])].
  letterDigit >> L --> [L], {isLetter(L)||isDigit(L)}.

  path:(cons[char]) => option[(cons[string],cons[char])].
  path >> P --> ident*"/">>P.

  main:()=>().
  main()=>valof{
    Txt = "iden45 "::cons[char];

    show ident(Txt);
    assert ("id32",[])?=ident("id32"::cons[char]);
    show ident("32"::cons[char]);
    assert ~ _?=ident("32"::cons[char]);

    show path("alpha/beta/gamma"::cons[char]);

    assert (["alpha","beta","gamma"],[]) ?= path("alpha/beta/gamma"::cons[char]);
    valis ()
  }
}
