rdf.sparql.expreval{
  import star.
  import rdf.triple.
  import rdf.sparql.query.
  import rdf.sparql.solution.

  /* Expression evaluation for FILTER/BIND/aggregate arguments. evalExpr's first parameter
     is the pattern evaluator (rdf.sparql.engine's evalPattern, partially applied to a
     graph) rather than an import of rdf.sparql.engine itself -- engine.star needs this
     module for FILTER/BIND, and this module needs engine.star's evaluator for
     EXISTS/NOT EXISTS, so the callback is threaded through as a plain function value to
     avoid a circular import (per the phased plan).

     v1 scope: the full operator set (comparisons, logic, arithmetic with int/float
     promotion), EBV, EXISTS/NOT EXISTS, and a solid subset of the ~65 builtins (STR, LANG,
     DATATYPE, the ISxxx type-test predicates, ABS, STRLEN, CONTAINS, STRSTARTS, STRENDS,
     SAMETERM, CONCAT, COALESCE, IF). Regex/replace/substr, date/time functions, the hash
     functions, UUID/rand, IRI/BNODE construction, RDF-star term functions
     (TRIPLE/SUBJECT/PREDICATE/OBJECT), UCASE/LCASE (no case-conversion primitive exists in
     this Star distribution's stdlib), and a few others raise a clear "not supported yet"
     error instead -- a deliberate, documented cut given the size of the full builtin set,
     not a silent gap. FILTER/BIND (engine.star) already treat any evaluation error as
     "not true"/propagate, per the SPARQL error-tolerance rules, so an unsupported builtin
     inside a FILTER fails that one solution rather than the whole query. */

  public evalExpr:((pattern)=>solutions throws string,mapping,expression) => concept throws string.
  evalExpr(_,M,.term(T)) => evalTerm(M,T).
  evalExpr(EvalP,M,.or(A,B)) => .bool(evalOr(EvalP,M,A,B)).
  evalExpr(EvalP,M,.and(A,B)) => .bool(evalAnd(EvalP,M,A,B)).
  evalExpr(EvalP,M,.not(A)) => .bool(~evalBoolOf(EvalP,M,A)).
  evalExpr(EvalP,M,.eq(A,B)) => .bool(termEq(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B))).
  evalExpr(EvalP,M,.ne(A,B)) => .bool(~termEq(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B))).
  evalExpr(EvalP,M,.lt(A,B)) => .bool(numCompare(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)) < 0).
  evalExpr(EvalP,M,.gt(A,B)) => .bool(numCompare(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)) > 0).
  evalExpr(EvalP,M,.le(A,B)) => .bool(numCompare(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)) =< 0).
  evalExpr(EvalP,M,.ge(A,B)) => .bool(numCompare(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)) >= 0).
  evalExpr(EvalP,M,.isIn(A,Bs)) => .bool(memberOf(evalExpr(EvalP,M,A),EvalP,M,Bs)).
  evalExpr(EvalP,M,.notIn(A,Bs)) => .bool(~memberOf(evalExpr(EvalP,M,A),EvalP,M,Bs)).
  evalExpr(EvalP,M,.add(A,B)) => numAdd(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)).
  evalExpr(EvalP,M,.sub(A,B)) => numSub(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)).
  evalExpr(EvalP,M,.mul(A,B)) => numMul(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)).
  evalExpr(EvalP,M,.div(A,B)) => numDiv(evalExpr(EvalP,M,A),evalExpr(EvalP,M,B)).
  evalExpr(EvalP,M,.pos(A)) => numPos(evalExpr(EvalP,M,A)).
  evalExpr(EvalP,M,.neg(A)) => numNeg(evalExpr(EvalP,M,A)).
  evalExpr(_,M,.bound(V)) => .bool(isBoundVar(M,V)).
  evalExpr(EvalP,M,.existsPattern(P)) => .bool(size(join(EvalP(P),[M])) > 0).
  evalExpr(EvalP,M,.notExists(P)) => .bool(size(join(EvalP(P),[M])) == 0).
  evalExpr(EvalP,M,.call(Nm,Args)) => evalCall(EvalP,M,Nm,Args).
  evalExpr(_,_,_) default => throw "this expression form is not supported yet".

  evalTerm:(mapping,term) => concept throws string.
  evalTerm(_,.literal(C)) => C.
  evalTerm(M,.var(V)) where C ?= M[V] => C.
  evalTerm(_,.var(V)) => throw "unbound variable in expression: ?$(V)".
  evalTerm(_,_) default => throw "this term form is not supported yet in expressions".

  isBoundVar:(mapping,string) => boolean.
  isBoundVar(M,V) => (_ ?= M[V] ?? .true || .false).

  memberOf:(concept,(pattern)=>solutions throws string,mapping,cons[expression]) => boolean throws string.
  memberOf(_,_,_,[]) => .false.
  memberOf(V,EvalP,M,[E,..Es]) => termEq(V,evalExpr(EvalP,M,E)) || memberOf(V,EvalP,M,Es).

  -- Effective Boolean Value, per https://www.w3.org/TR/sparql12-query/#ebv.
  public ebv:(concept) => boolean throws string.
  ebv(.bool(B)) => B.
  ebv(.int(N)) where N==0 => .false.
  ebv(.int(_)) => .true.
  ebv(.flt(N)) where N==0.0 => .false.
  ebv(.flt(_)) => .true.
  ebv(.text([.str(S)])) => ~(S=="").
  ebv(.text([])) => .false.
  ebv(.text(_)) => .true.
  ebv(_) default => throw "operand has no Effective Boolean Value".

  evalBoolOf:((pattern)=>solutions throws string,mapping,expression) => boolean throws string.
  evalBoolOf(EvalP,M,E) => ebv(evalExpr(EvalP,M,E)).

  /* && / || have SPARQL-specific 3-valued-logic error handling, not plain short-circuit:
     an error on one side can be *masked* if the other side alone already determines the
     result (false for &&, true for ||) -- see https://www.w3.org/TR/sparql12-query/#OperatorMapping.
     The exact error message isn't preserved through the mask-check path (the spec allows
     "an implementation may choose" which error to surface when both sides fail); that's a
     deliberate simplification, not a bug. */
  evalAnd:((pattern)=>solutions throws string,mapping,expression,expression) => boolean throws string.
  evalAnd(EvalP,M,A,B) => valof{
    try{
      if evalBoolOf(EvalP,M,A) then
        valis evalBoolOf(EvalP,M,B)
      else
        valis .false
    } catch {
      _ do valis andLeftErrored(EvalP,M,B)
    }
  }

  andLeftErrored:((pattern)=>solutions throws string,mapping,expression) => boolean throws string.
  andLeftErrored(EvalP,M,B) => valof{
    try{
      if evalBoolOf(EvalP,M,B) then
        throw "&&: left operand errored and right operand is not false"
      else
        valis .false
    } catch {
      _ do throw "&&: both operands failed to evaluate"
    }
  }

  evalOr:((pattern)=>solutions throws string,mapping,expression,expression) => boolean throws string.
  evalOr(EvalP,M,A,B) => valof{
    try{
      if evalBoolOf(EvalP,M,A) then
        valis .true
      else
        valis evalBoolOf(EvalP,M,B)
    } catch {
      _ do valis orLeftErrored(EvalP,M,B)
    }
  }

  orLeftErrored:((pattern)=>solutions throws string,mapping,expression) => boolean throws string.
  orLeftErrored(EvalP,M,B) => valof{
    try{
      if evalBoolOf(EvalP,M,B) then
        valis .true
      else
        throw "||: left operand errored and right operand is not true"
    } catch {
      _ do throw "||: both operands failed to evaluate"
    }
  }

  /* SPARQL term equality (=), simplified: same-shape literals compare structurally (reusing
     rdf.triple's equality[concept]), with int/float cross-promotion for numeric equality --
     no throw on genuinely incomparable types (e.g. a string vs a URI), just false, unlike
     strict SPARQL "=" (which type-errors there); a documented simplification given scope. */
  termEq:(concept,concept) => boolean.
  termEq(.int(A),.flt(B)) => valof{
    try{
      valis intToFltUnsafe(A)==B
    } catch {
      _ do valis .false
    }
  }
  termEq(.flt(A),.int(B)) => valof{
    try{
      valis A==intToFltUnsafe(B)
    } catch {
      _ do valis .false
    }
  }
  termEq(C1,C2) default => C1==C2.

  numCompare:(concept,concept) => integer throws string.
  numCompare(.int(A),.int(B)) => intCmp(A,B).
  numCompare(.int(A),.flt(B)) => fltCmp(intToFlt(A),B).
  numCompare(.flt(A),.int(B)) => fltCmp(A,intToFlt(B)).
  numCompare(.flt(A),.flt(B)) => fltCmp(A,B).
  numCompare(_,_) default => throw "comparison operands are not numeric".

  intCmp(A,B) where A<B => -1.
  intCmp(A,B) where A>B => 1.
  intCmp(_,_) => 0.

  fltCmp(A,B) where A<B => -1.
  fltCmp(A,B) where A>B => 1.
  fltCmp(_,_) => 0.

  numAdd:(concept,concept) => concept throws string.
  numAdd(.int(A),.int(B)) => .int(A+B).
  numAdd(.int(A),.flt(B)) => .flt(intToFlt(A)+B).
  numAdd(.flt(A),.int(B)) => .flt(A+intToFlt(B)).
  numAdd(.flt(A),.flt(B)) => .flt(A+B).
  numAdd(_,_) default => throw "+: operands are not numeric".

  numSub:(concept,concept) => concept throws string.
  numSub(.int(A),.int(B)) => .int(A-B).
  numSub(.int(A),.flt(B)) => .flt(intToFlt(A)-B).
  numSub(.flt(A),.int(B)) => .flt(A-intToFlt(B)).
  numSub(.flt(A),.flt(B)) => .flt(A-B).
  numSub(_,_) default => throw "-: operands are not numeric".

  numMul:(concept,concept) => concept throws string.
  numMul(.int(A),.int(B)) => .int(A*B).
  numMul(.int(A),.flt(B)) => .flt(intToFlt(A)*B).
  numMul(.flt(A),.int(B)) => .flt(A*intToFlt(B)).
  numMul(.flt(A),.flt(B)) => .flt(A*B).
  numMul(_,_) default => throw "*: operands are not numeric".

  -- SPARQL division always yields a fractional result -- promote both operands to float
  -- rather than truncating integer division.
  numDiv:(concept,concept) => concept throws string.
  numDiv(A,B) => valof{
    try{
      valis numDivRaw(A,B)
    } catch {
      _ do throw "/: division error (divide by zero?)"
    }
  }

  numDivRaw:(concept,concept) => concept throws exception.
  numDivRaw(.int(A),.int(B)) => .flt(intToFltViaExc(A)/intToFltViaExc(B)).
  numDivRaw(.int(A),.flt(B)) => .flt(intToFltViaExc(A)/B).
  numDivRaw(.flt(A),.int(B)) => .flt(A/intToFltViaExc(B)).
  numDivRaw(.flt(A),.flt(B)) => .flt(A/B).
  numDivRaw(_,_) default => throw .exception("/: operands are not numeric").

  intToFltViaExc:(integer) => float throws exception.
  intToFltViaExc(N) => N::float.

  numPos:(concept) => concept throws string.
  numPos(.int(A)) => .int(A).
  numPos(.flt(A)) => .flt(A).
  numPos(_) default => throw "unary +: operand is not numeric".

  numNeg:(concept) => concept throws string.
  numNeg(.int(A)) => .int(-A).
  numNeg(.flt(A)) => .flt(-A).
  numNeg(_) default => throw "unary -: operand is not numeric".

  numAbs:(concept) => concept throws string.
  numAbs(.int(A)) => .int(abs(A)).
  numAbs(.flt(A)) => .flt(abs(A)).
  numAbs(_) default => throw "abs(): operand is not numeric".

  intToFlt:(integer) => float throws string.
  intToFlt(N) => valof{
    try{
      valis intToFltUnsafe(N)
    } catch {
      _ do throw "integer out of range for numeric conversion"
    }
  }

  intToFltUnsafe:(integer) => float throws exception.
  intToFltUnsafe(N) => N::float.

  -- ===== builtin function calls =====

  evalCall:((pattern)=>solutions throws string,mapping,string,cons[expression]) => concept throws string.
  evalCall(EvalP,M,"str",[A]) => .text([.str(lexicalForm(evalExpr(EvalP,M,A)))]).
  evalCall(EvalP,M,"lang",[A]) => .text([.str(langOf(evalExpr(EvalP,M,A)))]).
  evalCall(EvalP,M,"datatype",[A]) => datatypeOf(evalExpr(EvalP,M,A)).
  evalCall(EvalP,M,"isiri",[A]) => .bool(isIriConcept(evalExpr(EvalP,M,A))).
  evalCall(EvalP,M,"isuri",[A]) => .bool(isIriConcept(evalExpr(EvalP,M,A))).
  evalCall(EvalP,M,"isblank",[A]) => .bool(isBlankConcept(evalExpr(EvalP,M,A))).
  evalCall(EvalP,M,"isliteral",[A]) => .bool(isLiteralConcept(evalExpr(EvalP,M,A))).
  evalCall(EvalP,M,"isnumeric",[A]) => .bool(isNumericConcept(evalExpr(EvalP,M,A))).
  evalCall(EvalP,M,"abs",[A]) => numAbs(evalExpr(EvalP,M,A)).
  evalCall(EvalP,M,"strlen",[A]) => .int(size(textOf(evalExpr(EvalP,M,A)))).
  evalCall(EvalP,M,"contains",[A,B]) =>
    .bool(isSome(strFind(textOf(evalExpr(EvalP,M,A)),textOf(evalExpr(EvalP,M,B)),0))).
  evalCall(EvalP,M,"strstarts",[A,B]) =>
    .bool(strPrefix(textOf(evalExpr(EvalP,M,B)),textOf(evalExpr(EvalP,M,A)))).
  evalCall(EvalP,M,"strends",[A,B]) =>
    .bool(strSuffix(textOf(evalExpr(EvalP,M,B)),textOf(evalExpr(EvalP,M,A)))).
  evalCall(EvalP,M,"sameterm",[A,B]) => .bool(evalExpr(EvalP,M,A)==evalExpr(EvalP,M,B)).
  evalCall(EvalP,M,"concat",Es) => .text([.str(concatAll(EvalP,M,Es))]).
  evalCall(EvalP,M,"coalesce",Es) => firstOk(EvalP,M,Es).
  evalCall(EvalP,M,"if",[A,B,C]) => (evalBoolOf(EvalP,M,A) ?? evalExpr(EvalP,M,B) || evalExpr(EvalP,M,C)).
  evalCall(_,_,Nm,_) default => throw "$(Nm)() is not supported yet".

  lexicalForm:(concept) => string throws string.
  lexicalForm(.text([.str(S)])) => S.
  lexicalForm(.langText([.str(S)],_)) => S.
  lexicalForm(.typedText([.str(S)],_)) => S.
  lexicalForm(.int(N)) => disp(N).
  lexicalForm(.flt(N)) => disp(N).
  lexicalForm(.bool(B)) => disp(B).
  lexicalForm(.uri(U)) => U.
  lexicalForm(.named(P,S)) => P++":"++S.
  lexicalForm(_) default => throw "str(): unsupported term form".

  textOf:(concept) => string throws string.
  textOf(.text([.str(S)])) => S.
  textOf(.langText([.str(S)],_)) => S.
  textOf(.typedText([.str(S)],_)) => S.
  textOf(_) default => throw "expected a simple string literal".

  langOf:(concept) => string.
  langOf(.langText(_,L)) => L.
  langOf(_) default => "".

  datatypeOf:(concept) => concept throws string.
  datatypeOf(.int(_)) => .uri("http://www.w3.org/2001/XMLSchema#integer").
  datatypeOf(.flt(_)) => .uri("http://www.w3.org/2001/XMLSchema#double").
  datatypeOf(.bool(_)) => .uri("http://www.w3.org/2001/XMLSchema#boolean").
  datatypeOf(.typedText(_,D)) => D.
  datatypeOf(.langText(_,_)) => .uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString").
  datatypeOf(.text(_)) => .uri("http://www.w3.org/2001/XMLSchema#string").
  datatypeOf(_) default => throw "datatype(): not a literal".

  isIriConcept:(concept) => boolean.
  isIriConcept(.uri(_)) => .true.
  isIriConcept(_) default => .false.

  isBlankConcept:(concept) => boolean.
  isBlankConcept(.anon(_)) => .true.
  isBlankConcept(_) default => .false.

  isLiteralConcept:(concept) => boolean.
  isLiteralConcept(.int(_)) => .true.
  isLiteralConcept(.flt(_)) => .true.
  isLiteralConcept(.bool(_)) => .true.
  isLiteralConcept(.text(_)) => .true.
  isLiteralConcept(.langText(_,_)) => .true.
  isLiteralConcept(.typedText(_,_)) => .true.
  isLiteralConcept(_) default => .false.

  isNumericConcept:(concept) => boolean.
  isNumericConcept(.int(_)) => .true.
  isNumericConcept(.flt(_)) => .true.
  isNumericConcept(_) default => .false.

  isSome:(option[integer]) => boolean.
  isSome(.some(_)) => .true.
  isSome(.none) => .false.

  strSuffix:(string,string) => boolean.
  strSuffix(Suffix,Str) => size(Suffix) =< size(Str) && subString(Str,size(Str)-size(Suffix),size(Suffix))==Suffix.

  concatAll:((pattern)=>solutions throws string,mapping,cons[expression]) => string throws string.
  concatAll(_,_,[]) => "".
  concatAll(EvalP,M,[E,..Es]) => textOf(evalExpr(EvalP,M,E))++concatAll(EvalP,M,Es).

  firstOk:((pattern)=>solutions throws string,mapping,cons[expression]) => concept throws string.
  firstOk(_,_,[]) => throw "COALESCE: every argument failed to evaluate".
  firstOk(EvalP,M,[E,..Es]) => valof{
    try{
      valis evalExpr(EvalP,M,E)
    } catch {
      _ do valis firstOk(EvalP,M,Es)
    }
  }
}
