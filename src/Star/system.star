star.system{
  import star.

  -- Access standard posix facilities, such as getenv/exec etc.

  public getenv:(string)=>option[string].
  getenv(Var) => _getenv(Var).

  public setenv:(string,string) => ().
  setenv(Ky,Vl) => valof{
    try{
      valis _setenv(Ky,Vl);
    } catch {
      _ => valis () -- ignore errors
    }
  }

  public envir:()=>cons[(string,string)].
  envir() => _envir().

  public shell:(string,cons[string],cons[(string,string)])=>integer throws errorCode.
  shell(Cmd,Args,Env) => _shell(Cmd,Args,Env).

}
