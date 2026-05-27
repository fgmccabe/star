test.coe1{
  import star.core.
  import star.coerce.

  public implementation coercion[float,integer->>exception] => {
    _coerce(Dx) => (try
      _flt2int(Dx)
      catch {
	| .eINVAL => throw .exception("#(_flt2str(Dx,8,`g`,.false)) not integral")
	| .eRANGE => throw .exception("#(_flt2str(Dx,8,`g`,.false)) out of range as integer")
      }
    )
  }
}
  
