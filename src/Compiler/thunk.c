/*
 * A thunk is a function/expression that is evaluated once, 
 * whence it replaces itself with its value.
 * A thunk looks like
 * var Ptn is thunk(Exp)
 *
 * Thunks are evaluated as zero-argument functions
 */


typedef struct _thunk_info *thunkPo;
typedef struct _thunk_info{
  varInfoPo thunkFree;
  
} ThunkInfo;

retCode thunkCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  thunkPo thunk = (thunkPo)cl;
  retCode ret = assignVar(loc,src,thunk->thunkFree,code);
}



//
// A thunk evaluates an expression once and replaces itself with 
// its value once computed.
//
// analogous to
//
/**
 * let{
 *   var E:()=>%t := eval;
 *   function eval():%t is valof {
 *     XX:%t is <exp>;
 *     E := K(XX);
 *     valis XX;
 *   };
 *   function KK(XX):()=>%t is (function():%t is XX)
 * } in (function():()=>%t is E());
 */


retCode compThunk(sxPo def,
		  lPo scan,lPo evac,lPo scav,lPo entryPoint,lPo catch,
		  uniChar *path,dictPo dict,
		  dictPo *fnDict,
		  mtdPo mtd)
{
  assemPo code = methodCode(mtd);

  assert(sxIsThunk(def));

  alignTo(code,POINTER_SIZE);
  constP(code,catch);			/* The table of catch blocks */
  constP(code,scan);			/* locals scanner */
  constP(code,scav);
  constP(code,evac);			/* Must be in this order */

  defineLbl(code,entryPoint);		/* This is the function entry point */

  lxPo args = sxFunArgs(def);
  long localsSize = ALIGN(countLocals(def),16);

  enterFun(code,localsSize);

  dictPo fDict = *fnDict = funDict(-localsSize,dict);

  lPo reentryPoint = currLbl(code,genSym(".R"));
  ExitLabel reentry = {.name=name,.cont=jumpCont,.cl=reentryPoint};

  // Set up the special combination return
  Combo thunkCombo = {.cont1=assignVar,.cl1=&thunkVar,.cont2=returnCont,.cl2=Null};
      
  // Compile the program body
  sxPo resType = Null;
  retCode status = compileExp(sxThunkExp(def),&resType,
			      path,fDict,dict,&reentry,mtd,
			      thunkCont,thunkVar);

  // Generate the locals scanners table
  genLocalScanner(name,scan,fDict,mtd);

  return status;
}


