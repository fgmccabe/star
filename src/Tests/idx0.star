test.idx0{
  import star.
  import star.ideal.

  -- Basic test of ideal hash trees.
/*
  t0 : ideal[integer,string].
  t0 = ihEmpty.

  t1 = insertIdeal(t0,45,"a").

  t2 = insertIdeal(t1,46,"b").

  t3 = insertIdeal(t2,47,"c").

  t4 = insertIdeal(t3,48,"d").

  t5 = insertIdeal(t4,49,"e").

  t6 = insertIdeal(t5,50,"f").

  t7 = insertIdeal(t6,51,"g").

  t8 = insertIdeal(t7,52,"h").

  t9 = insertIdeal(t8,53,"i").

  show disp(t9).
*/
  zip:all e ~~ (list[e],integer,integer,ideal[integer,e])=>ideal[integer,e].
  zip([],_,_,T) => T.
  zip([V,..L],Ix,Inc,T) => zip(L,Ix+Inc,Inc,insertIdeal(T,Ix,V)).

  a1 = zip(["a","b","c","d","e","f","g","h","i","j","k","l"],0,1,ihEmpty).

  show disp(a1).

  a2 = zip(["m"],12,1,a1).

  show disp(a2).

  u0 = zip(["a","b","c"],0,4,ihEmpty).

--  show disp(u0).

  u1 = insertIdeal(u0,12,"d").

  show disp(u1).


  show disp(zip(["a","b","c","d","e","f","g","h","i"],0,1,ihEmpty)).

  show disp(zip(["a","b","c","d","e","f","g","h","i","j","k","l"],0,1,ihEmpty)).

  show disp(zip(["a","b","c","d","e","f","g","h","i","j","k","l","m"],0,1,ihEmpty)).

  show disp(zip(["a","b","c","d","e","f","g","h","i","j","k","l","m",
                 "n","o","p","q","r","s","t","u","v","w","x","y","z"],0,1,ihEmpty)).

}
