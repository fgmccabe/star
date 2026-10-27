test.e1{
  box ::= exists r ~~ .box(r).

  wrap:all x ~~ (x) => box.
  wrap(X) => .box(X).

  main:(){}.
  main(){
  }
}
