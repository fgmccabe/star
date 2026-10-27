test.e2{
  import star.
  import star.assert.

  proto[i] ::= exists r ~~ .query((i)=>r, (r){}).

  send:all i,x ~~ ((i)=>x, (x){}) => proto[i].
  send(Q, R) => .query(Q, R).

  handle:all i ~~ (proto[i], i){}.
  handle(.query(Q, R), V){
    R(Q(V))
  }

  main:(){}.
  main(){
    Res := 0;
    Msg = send((X:integer)=>X+1, (R){ Res := R });
    handle(Msg, 41);
    assert Res! == 42
  }
}
