star.date{
  -- Date and time functions
  import star.

  public time ::=  .time(float). -- Time since 1st Jan 1970

  public today:()=>time.
  today() => .time(_today()).

  public now:()=>time.
  now() => .time(_now()).

  public ticks:()=>integer.
  ticks() => _ticks().

  public implementation display[time] => {
    disp(.time(Tm)) => (try _formattime(Tm,"www dd/mmm/yyyy hh:MM:SS aa") catch {_ => ""}).
  }

  public implementation format[time] => {
    _format(.time(Tm),F) => (try _formattime(Tm,F) catch {_ => "invalid format"}).
  }

  public implementation equality[time] => {
    .time(T1) == .time(T2) => T1==T2.
    _ == _ default => .false
  }

  public implementation comp[time] => {
    .time(T1) < .time(T2) => T1<T2.
    _ < _ default => .false.

    .time(T1) >= .time(T2) => T1>=T2.
    _ >= _ default => .false
  }

  public timeDiff:(time,time)=>float.
  timeDiff(.time(T1),.time(T2)) => T1-T2.

  public timeDelta:(time,float) => time.
  timeDelta(.time(T1),D) => .time(T1+D).

  public parseTime:(string,string) => option[time].
  parseTime(T,F) where Tm ?= _parsetime(T,F) => .some(.time(Tm)).
  parseTime(_,_) default => .none.
}
  
    
