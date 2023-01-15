star.date{
  -- Date and time functions
  import star.

  public time ::= private .time(float). -- Time since 1st Jan 1970

  public date ::= date{
    year:integer.
    month:integer.
    day:integer.
    dow:integer.
    hour:integer.
    min:integer.
    sec:float.
    tz:integer.
  }

  public today:()=>date.
  today() => valof{
    (Dw,Dy,Mn,Yr,Hr,Min,Sec,Tz) = _time2date(_today());
    valis date{
      year = Yr.
      month = Mn.
      day = Dy.
      dow = Dw.
      hour = Hr.
      min = Min.
      sec = Sec.
      tz = Tz
    }
  }

  public now:()=>time.
  now() => .time(_now()).

  public ticks:()=>integer.
  ticks() => _ticks().

  public implementation coercion[time,date] => {
    _coerce(.time(Tm)) => valof{
      (Dw,Dy,Mn,Yr,Hr,Min,Sec,Tz) = _time2date(Tm);
      valis ?date{
	year = Yr.
	month = Mn.
	day = Dy.
	dow = Dw.
	hour = Hr.
	min = Min.
	sec = Sec.
	tz = Tz
      }
    }
  }

  public implementation coercion[date,time] => {
    _coerce(date{
	year = Yr.
	month = Mn.
	day = Dy.
	dow = Dw.
	hour = Hr.
	min = Min.
	sec = Sec.
	tz = Tz}) => ?.time(_date2time(Yr,Mn,Dy,Hr,Min,Sec,Tz))
  }

  public implementation display[date] => {
    disp(date{year = Yr.
	month = Mn.
	day = Dy.
	dow = Dw.
	hour = Hr.
	min = Min.
	sec = Sec.
	tz = Tz}) => _fmttime(_date2time(Yr,Mn,Dy,Hr,Min,Sec,Tz),"%a %e/%b/%Y %X")
  }

  public implementation format[date] => {
    frmt(date{year = Yr.
	month = Mn.
	day = Dy.
	dow = Dw.
	hour = Hr.
	min = Min.
	sec = Sec.
	tz = Tz},F) => _fmttime(_date2time(Yr,Mn,Dy,Hr,Min,Sec,Tz),F)
  }

  public implementation display[time] => {
    disp(.time(Tm)) => _fmttime(Tm,"%a %e/%b/%Y %X")
  }

  public implementation format[time] => {
    frmt(.time(Tm),F) => _fmttime(Tm,F)
  }
}
  
    
