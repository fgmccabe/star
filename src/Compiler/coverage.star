star.compiler.coverage{
  import star.

  import star.compiler.canon.
  import star.compiler.canondeps.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  public checkPtnCoverage:(cons[canon],dict,tipe) => ().
  checkPtnCoverage(Ptns,Map,Tp) => valof{
    if CnsMap ?= findConstructors(Tp,Map) then{
--      showMsg("Check $(Ptns) against constructors $(CnsMap) for $(Tp)");
      for CnNm->CnTp in CnsMap do{
--	showMsg("Look for covereage of $(CnNm)->$(CnTp)");

	ArgSet = { Args | Ptn in Ptns && .apply(_,.enm(_,CnNm,_),Args,_) .= Ptn };
--	showMsg("$(ArgSet)");
	checkPtnsCoverage(ArgSet,Map);
      }
    };
    valis ()
  }

  checkPtnsCoverage:(cons[cons[canon]],dict)=>().
  checkPtnsCoverage([],Map) => ().
  checkPtnsCoverage([A,..As],Map) => valof{
    checkPtnCoverage(A,Map,typeOf(A));
    valis checkPtnsCoverage(As,Map);
  }
}    

  
