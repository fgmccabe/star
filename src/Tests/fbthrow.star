test.fbthrow{
  import star.
  import star.assert.

  /* A fiber whose body throws, uncaught by the fiber itself; the resumer
  catches the exception via resume's new throwing form.
  */

  body:((integer)=>>integer throws string,integer) => integer throws string.
  body(this,E) => valof{
    if E>5 then
      throw "too big"
    else
    valis 3*E
  }

  f:(integer) => integer throws string.
  f(X) => valof{
    valis _fiber(body) resume X
  }

  body2:((integer)=>>integer throws string,integer) => integer throws string.
  body2(this,E) => valof{
    if E>5 then
      this throw "too big"
    else
    valis 3*E
  }

  f2:(integer) => integer throws string.
  f2(X) => valof{
    valis _fiber(body2) resume X
  }

  -- Expression-form Fb throw E inside a non-throwing helper function
  hlp:((integer)=>>integer throws string,integer) => integer.
  hlp(Fb,E) => (E>5 ?? Fb throw "too big expr" || 3*E).

  body3:((integer)=>>integer throws string,integer) => integer throws string.
  body3(this,E) => hlp(this,E).

  f3:(integer) => integer throws string.
  f3(X) => _fiber(body3) resume X.

  -- Fb throw E bypasses an inner try/catch inside the fiber itself
  bodyBypass:((integer)=>>integer throws string,integer) => integer throws string.
  bodyBypass(this,E) => valof{
    try{
      if E>5 then
	this throw "bypassed inner catch"
      else
      valis 3*E
    } catch {
      _ do valis -1
    }
  }

  fBypass:(integer) => integer throws string.
  fBypass(X) => _fiber(bodyBypass) resume X.

  -- Suspending values then retiring with Fb throw E
  mkIter:all e ~~ (cons[e]) => (()=>>e throws string).
  mkIter(Lst) => _fiber((this,_) => valof{
    LL := Lst;
    while .cons(X,Els).=LL! do{
      LL := Els;
      this suspend X
    };
    this throw "exhausted"
  }).

  main:(){}.
  main(){
    try{
      assert f(2)==6;
      _ = f(10);
      unreachable;
    } catch {
      E do {
	assert E=="too big"
      }
    };
    try{
      assert f2(2)==6;
      _ = f2(10);
      unreachable;
    } catch {
      E do {
	assert E=="too big"
      }
    };
    try{
      assert f3(2)==6;
      _ = f3(10);
      unreachable;
    } catch {
      E do {
	assert E=="too big expr"
      }
    };
    try{
      assert fBypass(2)==6;
      _ = fBypass(10);
      unreachable;
    } catch {
      E do {
	assert E=="bypassed inner catch"
      }
    };
    It = mkIter([10,20]);
    try{
      assert (It resume ())==10;
      assert (It resume ())==20;
      _ = It resume ();
      unreachable
    } catch {
      E do {
	assert E=="exhausted"
      }
    };
    showMsg("Ok")
  }
}
