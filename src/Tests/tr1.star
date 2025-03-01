test.tr1{
  -- new form of try/catch handling
  import star.
  import star.assert.
  
  isOdd:(integer) => either[boolean,string].
  isOdd(X) => (X.&.1==0 ?? fail "$(X) is not odd" || result .true).

  checkMe:(integer) => boolean.
  checkMe(X) => (try
    ?isOdd(X)
    catch {
      Msg => valof{
	showMsg(Msg);
	valis .false
      }
    }).

  main:()=>().
  main()=>valof{
    show checkMe(3);
    show checkMe(4);

    assert checkMe(3);
    assert ~checkMe(4);
    valis ()
  }
}
  
