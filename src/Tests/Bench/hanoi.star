test.bench.hanoi{
  import star.
  import star.assert.
  import test.lib.timer.

  tower::=tower{name:char. pile:cons[integer]}.

  implementation display[tower] => {
    disp(T) => "Tower: $(T.name)\:$(T.pile)"
  }

  build(N,K) => tower{ name=N. pile=iota(1,K+1)}.

  move ::= .move(integer,char,char).

  implementation display[move] => {
    disp(.move(D,F,T)) => "Move $(D) from $(F) to $(T)"
  }

  implementation sizeable[tower] => {
    size(T) => size(T.pile).
    isEmpty(T) => isEmpty(T.pile)
  }

  moveTopDisk:(tower,tower,cons[move])=>(tower,tower,cons[move]).
  moveTopDisk(F,T,Mvs) where [D,..Ds].=F.pile => 
    (F.pile=Ds,T.pile=[D,..T.pile],[.move(D,F.name,T.name),..Mvs]).

  moveDisks:(integer,tower,tower,tower,cons[move])=>(tower,tower,tower,cons[move]).
  moveDisks(1,F,I,T,M) => valof{
    (F1,T1,M1) = moveTopDisk(F,T,M);
    valis (F1,I,T1,M1)
  }
  moveDisks(Cx,F,I,T,M) => valof{
    (F1,T1,I1,M1) = moveDisks(Cx-1,F,T,I,M);
    (F2,T2,M2) = moveTopDisk(F1,T1,M1);
    (I2,F3,T3,M3) = moveDisks(Cx-1,I1,F2,T2,M2);
    valis (F3,I2,T3,M3)
  }

  hanoi:(integer)=>integer.
  hanoi(Cx) => valof{
    towerA = build(`A`,Cx);
    towerB = build(`B`,0);
    towerC = build(`C`,0);
    (A,B,C,Ms) = moveDisks(Cx,towerA,towerB,towerC,[]);
    valis [|Ms|]
  }

  test(count) => 
    verfy(hanoi(count),count).

  verfy(8191,13) => .true.
  verfy(Mvs,Cnt) => Mvs==(1.<<.Cnt)-1.

  main:(integer){}.
  main(Cnt){
    timer = timer_start((1.<<.Cnt)-1, "Towers benchmark");

    assert test(Cnt);

    timer_finish(timer);
  }
}
