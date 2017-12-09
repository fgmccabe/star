import star;

ports is package{
  import actors;
  
  type port of %s is port{
    _notify has type action(action(%s));
    _request has type action(action(%s),()=>quoted,()=>dictionary of (string,any));
    _query has type  %a~((%s)=>%a,()=>quoted,()=>dictionary of (string,any))=>%a;
  };
  
  #p0rt{?S} ==> let{ #$"Schema" is {actorTheta(S)}; } in port{
    _notify(Fn) do Fn(#$"Schema");
    _request(Fn,Qt,Fr) do Fn(#$"Schema");
    _query(Fn,Qt,Fr) is Fn(#$"Schema");
  };
  
  type p0rt of %t is alias of port of %t;
  
  implementation speech over port of %schema determines %schema is {
    _query(P,Qf,Qt,Fr) is P._query(Qf,Qt,Fr);
    _request(P,Qf,Qt,Fr) do P._request(Qf,Qt,Fr);
    _notify(P,Np) do P._notify(Np);
  };
}