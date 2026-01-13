star.lex.re{
  import star.
  import star.location.
  import star.compiler.errors.
  import star.compiler.token.

  public re[c] ::= .epsilon
  | .class(cons[c])
  | .nclass(cons[c])
  | .choice(re[c],re[c])
  | .one(c)
  | .star(re[c])
  | .seq(re[c],re[c])
  | .bind(option[locn],string,re[c]).

  public implementation all c ~~ display[c] |= display[re[c]] => let{.
    dispRe(.epsilon) => "ε".
    dispRe(.class(L)) => interleave(L//disp,"|")*.
    dispRe(.nclass(L)) => "~#(interleave(L//disp,"|")*)".
    dispRe(.one(C)) => disp(C).
    dispRe(.star(N)) => "#(dispRe(N))*".
    dispRe(.seq(L,R)) => "#(dispRe(L))#(dispRe(R))".
    dispRe(.choice(L,R)) => "(#(dispRe(L))|#(dispChoice(R)))".
    dispRe(.bind(_Lc,Vr,N)) => "#(dispRe(N))>>#(Vr)".

    dispChoice(.choice(L,R)) => "#(dispRe(L))|#(dispChoice(R))".
    dispChoice(L) => dispRe(L).
  .} in {
    disp(N) => dispRe(N)
  }

  public implementation all c ~~ hashable[c] |= hashable[re[c]] => let{.
    hashR(.epsilon) => 0.
    hashR(.one(R)) => hash(R)*37+1.
    hashR(.class(L)) => foldLeft((C,H)=>H*37+hash(C),0,L).
    hashR(.nclass(L)) => foldLeft((C,H)=>H*37+hash(C),0,L).
    hashR(.star(R)) => hashR(R)*37+3.
    hashR(.choice(L,R)) => (hashR(L)*37+hashR(R))*37+7.
    hashR(.seq(L,R)) => (hashR(L)*37+hash(R))*37+5.
    hashR(.bind(_,_,R)) => hashR(R)
  .} in {
    hash = hashR
  }

  parseRe:(locn,string)=>option[re[char]].
  parseRe(Lc,Rs) => valof{
    Chrs = Rs::cons[char];
    (Re,Rest) = parseR(initSt(Lc,Chrs));
    if ~atEof(Rest) then{
      reportError("extra stuff at end of regular expression: $(Rest)",
	.some(makeLoc(Rest,Rest)));
      valis .none
    }
    else{
      valis .some(Re)
    }
  }
    
  parseR:(tokenState) => (re[char],tokenState).
  parseR(St) where (Nx,.some(Chr)) .= nextChr(St) => case Chr in {
    | `[` => parseMore(((Nx1,.some(`^`)).=nextChr(Nx) ?? valof{
	  valis parseCharClass(Nx1,(Chrs)=>.nclass(Chrs),.nil);
	} || parseCharClass(Nx,(Chrs)=>.class(Chrs),.nil)))
    | `(` => parseMore(lookFor(`)`,parseR(Nx)))
    | Ch where isLetter(Ch) => parseMore((.one(Ch),Nx))
    | _ default => valof{
      reportError("Invalid char $(Chr) in regular expression",.some(makeLoc(St,Nx)));
      valis parseR(Nx)
    }
  }
  parseR(St) default => (.epsilon,St).

  parseMore((SoFar,St)) where (Nx,.some(Chr)).=nextChr(St) => case Chr in {
    | `|` => parseUnion(SoFar,Nx)
    | `*` => parseMore((.star(SoFar),Nx))
    | `?` => parseMore((.choice(SoFar,.epsilon),Nx))
    | `>` => valof{
      (St1,Idn) = readIden(Nx,[]);
      valis parseMore((.bind(.some(makeLoc(Nx,St1)),Idn,SoFar),St1))
    }
    | `)` => (SoFar,St)
    | _ default => valof{
      (Next,St1) = parseR(St);
      valis parseMore((.seq(SoFar,Next),St1))
    }
  }

  parseUnion(SoFar,St) where (Nx,.some(Chr)).=nextChr(St) => case Chr in {
    | `|` => valof{
      (Arm,Nx1) = parseR(Nx);
      valis parseUnion(.choice(SoFar,Arm),Nx1)
    }
    | _ => (SoFar,St)
  }

  lookFor(Rb,(Re,St)) where (Nx,.some(Chr)).=nextChr(St) => case Chr in {
    | Rb => (Re,Nx)
    | _ => valof{
      (Re1,St1) = parseR(St);
      valis lookFor(Rb,(.seq(Re,Re1),St1))
    }
  }

  parseCharClass:(tokenState,(cons[char])=>re[char],cons[char]) => (re[char],tokenState).
  parseCharClass(St,Wrp,SoFar) where (Nx,.some(Chr)) .= nextChr(St) => case Chr in {
    | `\\` => ((St1,.some(Esc)) .= charRef(St) ??
      parseCharClass(St1,Wrp,[Esc,..SoFar]) ||
      parseCharClass(Nx,Wrp,SoFar))
    | `]` => (Wrp(SoFar),Nx)
    | _ default => parseCharClass(Nx,Wrp,[Chr,..SoFar])
  }.
  parseCharClass(St,Wrp,SoFar) => valof{
    reportError("incomplete char class",.some(makeLoc(St,St)));
    valis (Wrp(SoFar),St)
  }

  charRef(St) where Nx ?= lookingAt(St,[`\\`]) && (Nxt,.some(Ch)) .= nextChr(Nx) => backslashRef(Nxt,Ch).
  charRef(St) => nextChr(St).

  backslashRef:(tokenState,char) => (tokenState,option[char]).
  backslashRef(St,`a`) => (St,.some(`\a`)).
  backslashRef(St,`b`) => (St,.some(`\b`)).
  backslashRef(St,`e`) => (St,.some(`\e`)).
  backslashRef(St,`t`) => (St,.some(`\t`)).
  backslashRef(St,`n`) => (St,.some(`\n`)).
  backslashRef(St,`r`) => (St,.some(`\r`)).
  backslashRef(St,`u`) where (St1,.some(Hx)) .= hexChars(St,0) &&
      Stx ?= lookingAt(St1,[`;`]) => (Stx,.some(Hx:?char)).
  backslashRef(St,Ch) => (St,.some(Ch)).

  hexChars:(tokenState,integer) => (tokenState,option[integer]).

  hexChars(St,Hx) where Hd?=hedChar(St) && Dg?=isHexDigit(Hd) =>
    hexChars(nxtSt(St),Hx*16+Dg).
      hexChars(St,Hx) => (St,.some(Hx)).

  isIdentifierStart(Ch) => (Ch==`_` || isLetter(Ch)).

  readIden:(tokenState,cons[char]) => (tokenState,string).
  readIden(St,SoF) where (Nx,.some(Chr)) .= nextChr(St) && (isIdentifierStart(Chr) || _isNdChar(Chr)) =>
    readIden(Nx,[Chr,..SoF]).
  readIden(St,SoF) default => (St,reverse(SoF)::string).
}
