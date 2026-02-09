test.bench.tower{
  import star.
  import star.assert.
  import test.lib.timer.

  tower::=tower{name:char. pile:cons[integer]}.

  move ::= .move(integer,char,char).

  moveTopDisk:(tower,tower,tower,cons[move])=>(tower,tower,tower,cons[move]).
  moveTopDisk(F,I,T,Mvs) where [D,..Ds].=F.pile => (F.pile=Ds,I,T.pile=[D,..T.pile],
    [.move(D,F.name,T.name),..Mvs].

