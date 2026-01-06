star.log{
  import star.core.
  import star.coerce.

  public traceCall:all x ~~ display[x] |= (string,boolean,x) => x.
  traceCall(M,Flg,X) => valof{
    if Flg then
      _show("\e[34m#(M)\e[0m - $(X)");
    valis X
  }

  public logMsg:all l ~~ loggable[l] |= (l,string)=>().
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

  public implementation coercion[string,logLevel->>exception] => {
    _coerce(Nm) => (
      case Nm in {
	| "finest" => .finest
	| "finer" => .finer
	| "fine" => .fine
	| "config" => .config
	| "info" => .info
	| "warning" => .warning
	| "severe" => .severe
	| _ default => throw .exception("[#(Nm)] not a valid log level")
      }
    )
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
    try{
      if L ?= _getenv("LOGLEVEL") then
	valis _coerce(L)
      else
      valis .severe
    } catch { _ do valis .severe }
  }

  public contract all x ~~ loggable[x] ::= {
    isLogging:(x)=>boolean.
  }

  public implementation loggable[logLevel] => {
    isLogging(Lvl) => Lvl >= currentLogLevel().
  }
}
  
