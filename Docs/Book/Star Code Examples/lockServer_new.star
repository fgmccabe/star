-- See/Copyright Reppy, "Concurrent Programming in ML", Chapter 4

lockServer is package {
  import cmlLib;
  
  type lockid is alias of integer;
  
  type req_msg is
    ACQUIRE {
      lockid has type lockid;
      replyCh has type channel of Unit;
      abortRv has type rv of Unit;
    } or
    RELEASE {
      lockid has type lockid;
    };
    
  type lockServer is alias of channel of req_msg;
  
  acquireLockRv has type (lockServer, lockid) => rv of Unit;
  
  acquireLockRv(reqCh, lockid) is
    nack abortRv in
      let {
        replyCh is channel();
        { spawn {
            send ACQUIRE { lockid = lockid; replyCh = replyCh; abortRv = abortRv } to reqCh;
        } }
      } in (recvRv from replyCh)
      ));
      
  releaseLock has type action(lockServer, lockid)
  releaseLock(reqCh, lockid) do {
    send RELEASE { lockid = lockid; } to reqCh;
  }
  
  mkLockServer has type () => lockServer;
  
  mkLockServer() is let {
    replyToAcquire((replyCh, abortRv)) is
      await(choose {
        wrap sendRv Unit to replyCh with true;
        wrap abortRv with false;
      });
      
    _requestChannel is mk_channel();
    
    serve has type action(ref dictionary of (lockid, ref queue of ((channel of Unit, rv of Unit))));
    serve(locks) do {
      case receive from _requestChannel in {
        ACQUIRE { lockid = lockid; replyCh = replyCh; abortRv = abortRv } do {
        if lockid -> pending in locks then {
            -- known lock, and acquired
            pending := _cons((replyCh, abortRv), pending);
          } else {
            -- unknown lock, and available
            if replyToAcquire((replyCh, abortRv)) then {
              locks[lockid] := _cell(queue of {});
            }
          }
        };
        RELEASE { lockid = lockid } do {
          pending is ref locks[lockid];
          var assigned := false;
          while (not assigned) and (pending matches _pair(p, rest)) do {
            if replyToAcquire(p) then {
              pending := rest;
              assigned := true;
            }
            else
              pending := rest;
          };
          if not assigned then {
            remove locks[lockid];
          }
        };
      }; -- case
    };
    
    { spawn {
      var locks := dictionary of {};
      while true do {
        serve(ref locks);
      }
    } }
  } in (_requestChannel);
  
  
  main() {
    srv is mkLockServer();
    
    -- test 1: lock, wait, release
    _ is valof await(acquireLockRv(srv, 42));
    var test := 0;
    spawn {
      _ is valof await(acquireLockRv(srv, 42));
      test := 1;
    }
    sleep(100L);
    assert(test = 0);
    releaseLock(srv, 42);

    -- test 2: try two, then acquire the remaining one (making real use of NACK)
    _ is valof await(choose {
      wrap acquireLockRv(srv, 1) with await(acquireLockRv(srv, 2));
      wrap acquireLockRv(srv, 2) with await(acquireLockRv(srv, 1))
    })
    releaseLock(srv, 1);
    releaseLock(srv, 2);

  }
}
