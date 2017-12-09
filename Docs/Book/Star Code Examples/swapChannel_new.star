swapChannel is package {

  import cmlLib;
  
  type swapChannel of %a is alias of channel of ((%a, channel of %a))
  
  swapChannel has type () => swapChannel of %a
  swapChannel() is channel()
  
  swapRv has type (swapChannel of %a, %a) => rv of %a
  swapRv(sc, msg) is
    guard
      task {
        inCh is channel();
        recv_send is
          wrap recvRv from sc yielding (msgIn, outCh) with task { send(outCh, msg); valis msgIn }
          
        send_recv is 
          wrap sendRv (msg, inCh) to sc with receive from inCh
        valof choose { recv_send; send_recv }
      }
            
  main() {
    sc is swapChannel();
    scrv is swapRv(sc, "test");
    loops is 10000;
    spawn {
      for i in iota(1, loops, 1) do {
        _ is valof await(scrv);
      }
    }
    for i in iota(1, loops, 1) do {
      _ is valof await(scrv);
    }
  }
}
