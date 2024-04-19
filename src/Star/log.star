star.log{
  import star.core.
  import star.coerce.

  public traceCall:all x ~~ display[x] |: (string,x) => x.
  traceCall(M,X) => valof{
    _show("\e[34m#(M)\e[0m - $(X)");
    valis X
  }

  public logMsg:all l ~~ loggable[l] |: (l,string)=>().
  logMsg(Lvl,Msg) where isLogging(Lvl) => _logmsg(Msg).

  public showMsg:(string)=>().
  showMsg(Msg) => valof{
    valis _show(Msg)
  }

  public logLevel ::=
    .finest |
    .finer |
    .fine |
    .config |
    .info |
    .warning |
    .severe.

  public implementation display[logLevel] => {
    disp(.finest) => "finest".
    disp(.finer) => "finer".
    disp(.fine) => "fine".
    disp(.config) => "config".
    disp(.info) => "info".
    disp(.warning) => "warning".
    disp(.severe) => "severe".
  }

  public implementation coercion[string,logLevel] => {
    _coerce("finest") => .some(.finest).
    _coerce("finer") => .some(.finer).
    _coerce("fine") => .some(.fine).
    _coerce("config") => .some(.config).
    _coerce("info") => .some(.info).
    _coerce("warning") => .some(.warning).
    _coerce("severe") => .some(.severe).
    _coerce(_) => .none.
  }

  public implementation comp[logLevel] => {.
    .finest < .finer => .true.
    .finest < .fine => .true.
    .finest < .config => .true.
    .finest < .info => .true.
    .finest < .warning => .true.
    .finest < .severe => .true.
    .finer < .fine => .true.
    .finer < .config => .true.
    .finer < .info => .true.
    .finer < .warning => .true.
    .finer < .severe => .true.
    .fine < .config => .true.
    .fine < .info => .true.
    .fine < .warning => .true.
    .fine < .severe => .true.
    .config < .info => .true.
    .config < .warning => .true.
    .config < .severe => .true.
    .info < .warning => .true.
    .info < .severe => .true.
    .warning < .severe => .true.
    _ < _ default => .false.

    L1 >= L2 => ~ L2<L1.
  .}

  public currentLogLevel() => valof{
    if L ?= _getenv("LOGLEVEL") && Lvl ?= _coerce(L) then
      valis Lvl
    else
    valis .severe
  }

  public contract all x ~~ loggable[x] ::= {
    isLogging:(x)=>boolean.
  }

  public implementation loggable[logLevel] => {
    isLogging(Lvl) => Lvl >= currentLogLevel().
  }
}
  
