test.coe1{
  import star.core.
  import star.coerce.
  import star.assert.
  import star.arith.
  import star.log.

  public implementation coercion[float,integer->>exception] => {
    _coerce(Dx) => (try
      _flt2int(Dx)
      catch {
	| .eINVAL => throw .exception("#(_flt2str(Dx,8,`g`,.false)) not integral")
	| .eRANGE => throw .exception("#(_flt2str(Dx,8,`g`,.false)) out of range as integer")
      }
    )
  }

  main:(){}.
  main(){
    assert 16.0:?integer == 16;
    try{
      assert 12.4::integer == 12;
      unreachable
    } catch {
      .exception(Msg) do {
	showMsg(Msg)
      }
    }
  }
}
  
