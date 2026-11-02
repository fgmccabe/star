test.fbthrow{
  import star.
  import star.assert.

  /* A fiber whose body throws, uncaught by the fiber itself; the resumer
  catches the exception via resume's new throwing form.
  */

  body:(fiber[integer,integer] throws string,integer) => integer throws string.
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

  main:(){}.
  main(){
    try{
      show f(2);
      show f(10);
      show "should not reach here"
    } catch {
      E do showMsg("caught $(E)")
    }
  }
}
