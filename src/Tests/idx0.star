test.idx0{
  import star.
  import star.script.

  -- Basic test of ideal hash trees.

  zip:all e ~~ (cons[e],integer,integer,map[integer,e])=>map[integer,e].
  zip(L,Ix,Inc,T0) => snd(foldLeft((E,(I,T))=>(I+Inc,T[I->E]),(Ix,T0),L)).

  t0 : map[integer,string].
  t0 = [].

  t1 = t0[45->"a"].

  t2 = t1[46->"b"].

  t3 = t2[47->"c"].

  t4 = t3[48->"d"].

  t5 = t4[49->"e"].

  t6 = t5[50->"f"].

  t7 = t6[51->"g"].

  t8 = t7[52->"h"].

  t9 = t8[53->"i"].

  a1 = zip(["a","b","c","d","e","f","g","h","i","j","k","l"],0,1,[]).

  a2 = zip(["m"],12,1,a1).

  u0 = zip(["a","b","c"],0,4,[]).

  u1 = u0[12->"d"].

  letters:cons[string].
  letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
                 "n","o","p","q","r","s","t","u","v","w","x","y","z"].

  aa0 = zip(letters,0,1,[]).

  chek:all e ~~ (cons[e],integer,integer,(integer,e)=>boolean)=>boolean.
  chek(L,Ix,Inc,F) => snd(foldLeft((E,(I,S))=>(I+Inc,S&&F(I,E)),(Ix,.true),L)).

  unzip:all e ~~ (cons[e],integer,integer,map[integer,e])=>map[integer,e].
  unzip(L,Ix,Inc,T0) => snd(foldLeft((E,(I,T))=>(I+Inc,T[~I]),(Ix,T0),L)).

  main:()=>().
  main()=>valof{
    show t9;
    show a1;
    show a2;

    show u1;

    show u1[0];
    show u1[4];
    show u1[8];
    show u1[12];

    assert u1[5]==.none;
    assert u0[12] == .none;

    assert u1[12] == .some("d");

    show zip(["a","b","c","d","e","f","g","h","i"],0,1,[]);

    show zip(["a","b","c","d","e","f","g","h","i","j","k","l"],0,1,[]);

    show zip(["a","b","c","d","e","f","g","h","i","j","k","l","m"],0,1,[]);

    show aa0;
    show chek(["a","b","c","d"],0,4,(Ix,V)=>V?=u1[Ix]);

    assert chek(letters,0,1,(Ix,V)=>V?=aa0[Ix]);

    show u1[~12];

    show u1[~12][8];

    show unzip(["a","b","c"],0,4,[]);

    show unzip(["a","b","c"],0,4,u1);

    show ({12->"d"}:map[integer,string]);
    valis ()
  }

}
