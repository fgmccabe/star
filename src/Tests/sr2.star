test.sr2{
  import star.
  import star.assert.

  -- Simulate try-catch with shift/reset

 thro:all t,e ~~ (tag:tag(either[t,e]))|:(e)=>_.
 thro(X) => (shift k in .other(X)).

  ketch:all e,o ~~ (((tag:tag(either[e,o]))|:()=>e),(o)=>e) => e.
  ketch(F,K) => case (reset .either(F())) in {
    | .either(E) => E
    | .other(E) => K(E)
  }.

  choose:(tag:tag(either[string,string]))|:()=>string.
  choose() =>
    thro("there").

  main:()=>().
  main() => valof{
    assert ketch(()=>"hello",(_)=>"oops")=="hello";
    assert ketch(ζchoose,id) == "there";
  }
}
