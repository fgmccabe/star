/**
 * Monastication transformation.
 *
 * This transforms task { ... valof ... valis } expressions with
 * implicit monadic binding into expressions with explicit monadic
 * binding.
 */

monastication is package {
    type variable is alias of string;

/* source language */
    type expsrc is src_const(integer)                 /* constant */
      or src_variable(string)               /* variable */
      or src_lambda(variable, expsrc)       /* abstraction */
      or src_apply(expsrc, expsrc)          /* application */
      or src_cond(expsrc,expsrc,expsrc)
      or src_task(tasksrc)                  /* task { ... } expression */
      or src_valof(expsrc);                  /* valof expression */

    type tasksrc is
      src_valis(expsrc)                  /* valis */
      or src_call(expsrc,expsrc,tasksrc)    /* do something */
      or src_is(variable, expsrc, tasksrc)  /* x is e; <more> */
      or src_seq(list of tasksrc);

/* source language */
    type expdst is dst_const(integer)
      or dst_variable(string)
      or dst_lambda(variable, expdst)
      or dst_apply(expdst, expdst)
      or dst_cond(expdst,expdst,expdst)
      or dst_task(taskdst)
      or dst_perform(expdst);                /* top-level task execution */
    
    type taskdst is dst_valis(expdst)                   /* valis */
      or dst_call(expdst,expdst,taskdst)     /* call */
      or dst_bind(variable, expdst, taskdst) /* monad bind */
      or dst_is(variable, expdst, taskdst)   /* vanilla variable binding */
      or dst_sequence(list of taskdst);

/* transformation context */
    type dstcont is alias of cons of ((variable, expdst)); /* in reverse order */

/* transformation monad */
    type trafo of (%s, %t) is alias of ((%s) => (%t, %s));

    return has type (%t) => trafo of (%s, %t);
    return(x) is (function (s) is (x, s));

    bind has type (trafo of (%s, %t), (%t) => trafo of (%s, %v)) => 
      trafo of (%s, %v);
    bind(m, f) is
      (function (i0) is
	 let {
	   (v, s1) is m(i0);
	 } in
	 (f(v))(s1));

/*
 * We keep two bits of state:
 * - a counter for generating fresh variables
 *
 * - a transformation context that keeps track of transformed "valof"
 *   expressions that we pull out
 */
    type monastication_state is alias of (integer, dstcont);

/* monastication monad */
    type mona of %t is alias of trafo of (monastication_state, %t);

    empty_context has type dstcont;
    empty_context is nil;

/* run a monad value, extracting the final value and the context */
    run_mona has type (mona of %t) => (%t, dstcont);
    run_mona(m) is
      let {
	(v, (_, k)) is m((0, empty_context));
      } in
      (v, k);
    
/* generate a fresh variable */
    fresh_variable has type mona of variable;
    fresh_variable((i, k)) is
      ("_f" ++ display(i), (i + 1, k));

/* add a binding to the context */
    add_binding has type (expdst) => mona of variable;
    add_binding(edst) is
      bind(fresh_variable,
	   (function (v) is
	      (function ((i, k)) is
		 (v, (i, cons((v, edst), k))))));
      
/* apply context */
    apply_context has type (taskdst, dstcont) => taskdst;
    apply_context(tdst, nil) is tdst;
    apply_context(tdst, cons((v, r), rest)) is
      apply_context(dst_bind(v, r, tdst), rest);
    
/* transform a source task into a destination task */
    monasticate_task has type (tasksrc) => mona of taskdst;

    monasticate_task(src_valis(esrc)) is
      bind(monasticate_exp(esrc),
	   (function (edst) is 
	      return(dst_valis(edst))));
    
    monasticate_task(src_call(op,ar,nxt)) is
      bind(monasticate_exp(op),
	   (function(edop) is
	      bind(monasticate_exp(ar),
		   (function(edar) is 
		      bind(monasticate_task(nxt),
			   (function(nxtd) is
			      return(dst_call(edop,edar,nxtd))))))));
    
    monasticate_task(src_is(variable, esrc, tsrc)) is
      bind(monasticate_exp(esrc),
	   (function (edst) is
	      bind(monasticate_task(tsrc),
		   (function (tdst) is
		      return(dst_is(variable, edst, tdst))))));
    
/* transform a source expression into a destination expression */
    monasticate_exp has type (expsrc) => mona of expdst;
    
    monasticate_exp(src_const(i)) is
      return(dst_const(i));
    monasticate_exp(src_variable(n)) is
      return(dst_variable(n));
    monasticate_exp(src_lambda(n, esrc)) is
      return(dst_lambda(n, translate_exp(esrc)));
    monasticate_exp(src_apply(e0src, e1src)) is
      bind(monasticate_exp(e0src),
	   (function (e0dst) is
	      bind(monasticate_exp(e1src),
		   (function (e1dst) is
		      return(dst_apply(e0dst, e1dst))))));
    monasticate_exp(src_cond(srcT,srcTh,srcEl)) is
      bind(monasticate_exp(srcT),
	   (function(dstT) is
	      bind(monasticate_exp(srcTh),
		   (function(dstTh) is
		      bind(monasticate_exp(srcEl),
			   (function(dstEl) is 
			      return(dst_cond(dstT,dstTh,dstEl))))))));
    monasticate_exp(src_task(tsrc)) is
      return(translate_exp(src_task(tsrc)));
    monasticate_exp(src_valof(esrc)) is
      bind(monasticate_exp(esrc),
	   (function (edst) is
	      bind(add_binding(edst),
		   (function (v) is
		      return(dst_variable(v))))));
    monasticate_exp(E) is raise "cannot monasticate $E";

/* translate a source expression into a destination expression */
    translate_exp has type (expsrc) => expdst;
    translate_exp(src_const(i)) is dst_const(i);
    translate_exp(src_variable(n)) is dst_variable(n);
    translate_exp(src_lambda(n, esrc)) is dst_lambda(n, translate_exp(esrc));
    translate_exp(src_apply(e0src, e1src)) is dst_apply(translate_exp(e0src), translate_exp(e1src));
    translate_exp(src_cond(sTst,sTh,sEl)) is 
      dst_cond(translate_exp(sTst),translate_exp(sTh),translate_exp(sEl));
    translate_exp(src_task(tsrc)) is dst_task(translate_task(tsrc));
    translate_exp(src_valof(esrc)) is dst_perform(translate_exp(esrc));
    
/* translate a source task into a destination task */

    translate_task has type (tasksrc) => taskdst;
    translate_task(tsrc) is
      let {
	(tdst, k) is run_mona(monasticate_task(tsrc));
      } in
      apply_context(tdst, k);
    
    t has type action(expsrc);
    t(esrc) do {
	logMsg(info, display(esrc) ++ " => " ++ display(translate_exp(esrc)));
      };

    main() do {
	e1src is src_task(src_is("x", src_valof(src_task(src_valis(src_const(5)))), src_valis(src_variable("x"))));
	e1dst is translate_exp(e1src);
	logMsg(info, display(e1src) ++ " => " ++ display(e1dst));
	e2src is src_task(src_is("x", src_apply(src_valof(src_variable("f")), src_valof(src_task(src_valis(src_const(5))))), src_valis(src_variable("x"))));
	e2dst is translate_exp(e2src);
	logMsg(info, display(e2src) ++ " => " ++ display(e2dst));
	
	e3src is src_task(src_is("x",src_const(12),
				 src_call(src_variable("log"),src_variable("x"),
					  src_valis(src_variable("x")))));
	e3dst is translate_exp(e3src);
	logMsg(info, display(e3src) ++ " => " ++ display(e3dst));



	e4src is src_task(src_is("x", src_cond(src_variable("+"),
					       src_valof(src_variable("T")),
					       src_const(2)),
				 src_valis(src_apply(src_variable("x"),src_variable("y")))));
	e4dst is translate_exp(e4src);
	logMsg(info, display(e4src) ++ " => " ++ display(e4dst));
      };
  }
