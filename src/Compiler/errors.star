star.compiler.errors{
  import star.
  import star.compiler.location.

  reportMsg ::= .errorMsg(option[locn],string)
    | .warnMsg(option[locn],string).

  implementation display[reportMsg] => {
    disp(E) => case E in {
      .errorMsg(.none,Msg) => "\e[31merror\e[0m #(Msg)".
      .errorMsg(.some(Lc),Msg) => "\e[31error\e[0m #(Msg) at $(Lc)".
      .warnMsg(.none,Msg) => "\e[33mwarning\e[0m #(Msg)".
      .warnMsg(.some(Lc),Msg) => "\e[33mwarning\e[0m #(Msg) at $(Lc)".
    }
  }

  public displayErrorsAndWarnings() => "#(interleave(reports!//disp,"\n")*)\n$(countErrors()) errors\n$(countWarnings()) warnings".

  reports:ref cons[reportMsg].
  reports = ref [].

  trapCount = ref 0.

  public resetErrors() => valof{
    reports := [];
    valis ()
  }

  public errorFree:()=>boolean.
  errorFree() => countErrors()==0 && trapCount! == 0.

  public countErrors:()=>integer.
  countErrors() => foldLeft(countError,0,reports!).

  countError(.errorMsg(_,_),Ix)=>Ix+1.
  countError(_,Ix) default => Ix.

  public warningFree:()=>boolean.
  warningFree() => countWarnings()==0.

  public countWarnings:()=>integer.
  countWarnings() => foldLeft(countWarning,0,reports!).

  countWarning(.warnMsg(_,_),Ix)=>Ix+1.
  countWarning(_,Ix) default => Ix.

  public reportError:(string,option[locn]) => ().
  reportError(Msg,Lc) => valof{
    logMsg("\e[31merror $(countErrors()+1)\e[0m - #(Msg) at $(Lc)");
    reports := [.errorMsg(Lc,Msg),..reports!];
    valis ()
  }

  public reportWarning:(string,option[locn]) => ().
  reportWarning(Msg,Lc) => valof{
    logMsg(disp(.warnMsg(Lc,Msg)));
    reports := [.warnMsg(Lc,Msg),..reports!];
    valis ()
  }

  public countTraps:() => integer.
  countTraps() => trapCount!.
  
  public reportTrap:(string) => ().
  reportTrap(Msg) => valof{ 
    trapCount := trapCount!+1;
    logMsg("internal trap: #(Msg)");
    valis ()
  }
    
}
