test.u{
  import star.
  import star.script.

  Xs : cons[integer].
  Xs = [].

  resetBindings = action{
    if Rx in Xs then{
      resetBinding(Rx)
    };
    valis .false
  }

  resetBinding(_) => do{
    valis ()
  }

  XL = [E|E in Xs && E>3].

  main:()=>action[(),()].
  main() => do{
    assert size(XL)<size(Xs)
  }
}
  
