star.date{
  -- Date and time functions
  import star.

  public time ::= private .time(float) | private .never. -- Time since 1st Jan 1970

  public today:()=>time.
  today() => .time(_today()).

  public now:()=>time.
  now() => .time(_now()).

  public ticks:()=>integer.
  ticks() => _ticks().

  public implementation display[time] => {
    disp(.time(Tm)) => _fmttime(Tm,"%a %e/%b/%Y %X").
    disp(.never) => "never".
  }

  public implementation format[time] => {
    _format(.time(Tm),F) => _formattime(Tm,F).
    _format(.never,_) => "never".
  }
}
  
    
