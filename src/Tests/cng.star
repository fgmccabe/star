test.cng{
  import star.
  import star.script.

  Tg = tag().

  tgWrap[x,y] ::= tgWrap{ K:(x)=>>tgWrap[x,y]}.

  consWlk(.nil,Y) => Y(.none).
  consWlk(cons(H,T),Y) => valof do{
    Y(some(H));
    valis consWlk(T,Y)
  }

  mkYield(FC) =>
    (V) => let{
      cc = Tg cut K in tgWrap{K=K}.
    } in (case V in {
	.none => ().
	some(Vl) => FC.((cc,Vl))
    }).

/*  iterLoop(BF,IT) => let{
    loop() => let{
      Tg = tag().
      LC := Tg cut K in K.
*/
      
  main:()=>action[(),()].
  main()=>action{
    logMsg("hello");
  }
}
  
