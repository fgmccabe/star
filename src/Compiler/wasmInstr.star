star.compiler.wasm.instr{
  import star.
  import star.multi.
  import star.compiler.wasm.types.

  var ~> string.

  public wasmLbl ::= .lbl(string).

  public block_type ::=
    .VarBlockType(string) |
    .ValBlockType(option[value_type]) |
    .FunBlockType(cons[value_type],cons[value_type]).

  public implementation display[block_type] => {
    disp(.VarBlockType(Nm)) => "(type \$#(Nm))".
    disp(.ValBlockType(.none)) => "".
    disp(.ValBlockType(.some(Tp))) => disp(Tp).
    disp(.FunBlockType(Args,Res)) => "(param #((Args//(A)=>disp(A))*)) (result #((Res//(A)=>disp(A))*))".
  }

  all t,p ~~ memop[t,p] ::= memop{ty : t. align : integer. offset : integer. pack : p}

  loadop ~> memop[num_type, option[(pack_size, extension)]].
  storeop ~> memop[num_type, option[pack_size]].

  public pack_size ::= .Pack8 | .Pack16 | .Pack32 | .Pack64.
  public extension ::= .SX | .ZX.

  public wOp ::= 
    .Unreachable                       --  trap unconditionally
    | .Nop			       --  do nothing 
    | .Drop			       --  forget a value 
    | .Select                          --  branchless conditional 
    | .Block(option[wasmLbl],block_type, cons[wOp])  --  execute in sequence 
    | .Loop(option[wasmLbl],block_type, cons[wOp])   --  loop header 
    | .If(option[wasmLbl],block_type, cons[wOp], cons[wOp]) --  conditional 
    | .Br(wasmLbl)		       --  break to n-th surrounding label 
    | .BrIf(wasmLbl)		       --  conditional break 
    | .BrTable(cons[wasmLbl], wasmLbl) --  indexed break 
    | .Return			       --  break from function body 
    | .Call(var)		       --  call function 
    | .CallIndirect(var, var)	       --  call function through table 
    | .LocalGet(var)		       --  read local variable 
    | .LocalSet(var)		       --  write local variable 
    | .LocalTee(var)		       --  write local variable and keep value 
    | .GlobalGet(var)		       --  read global variable 
    | .GlobalSet(var)                  --  write global variable 
    | .TableGet(var)                   --  read table element 
    | .TableSet(var)                   --  write table element 
    | .TableSize(var)                  --  size of table 
    | .TableGrow(var)                  --  grow table 
    | .TableFill(var)                  --  fill table range with value 
    | .TableCopy(var, var)	       --  copy table range 
    | .TableInit(var, var)	       --  initialize table range from segment 
    | .ElemDrop(var)                   --  drop passive element segment 
    | .Load(loadop)		       --  read memory at address 
    | .Store(storeop)		       --  write memory at address 
    | .MemorySize		       --  size of memory 
    | .MemoryGrow		       --  grow memory 
    | .MemoryFill		       --  fill memory range with value 
    | .MemoryCopy		       --  copy memory ranges 
    | .MemoryInit(var)                 --  initialize memory range from segment 
    | .DataDrop(var)                   --  drop passive data segment 
    | .RefNull(ref_type)	       --  null reference 
    | .RefFunc(var)                    --  function reference 
    | .RefIsNull		       --  null test 
    | .Const(num_type)		       --  constant 
    | .IClz                            --  Clear to zero
    | .ICtz                            --  Count trailing zeroes
    | .Popcnt    
    | .IAdd(int_type)
    | .ISub(int_type)
    | .IMul(int_type)
    | .IDivS(int_type)
    | .IDivU(int_type)
    | .IRemS(int_type)
    | .IRemU(int_type)
    | .IAnd(int_type)
    | .IOr(int_type)
    | .IXor(int_type)
    | .IShl(int_type)
    | .IShrS(int_type)
    | .IShrU(int_type)
    | .IRotl(int_type)
    | .IRotr(int_type)
    | .IEqz(int_type)                            -- equal to zero
    | .IEq(int_type)                             -- Integer comparisons
    | .INe(int_type)
    | .ILtS(int_type)
    | .ILtU(int_type)
    | .IGtS(int_type)
    | .IGtU(int_type)
    | .ILeS(int_type)
    | .ILeU(int_type)
    | .IGeS(int_type)
    | .IGeU(int_type)
    | .FNeg(flt_type)                            -- Unary floating point
    | .FAbs(flt_type)
    | .FCeil(flt_type)
    | .Floor(flt_type)
    | .FTrunc(flt_type)
    | .FNearest(flt_type)
    | .FSqrt(flt_type)
    | .FEq(flt_type)                              -- Binary floating point
    | .FNe(flt_type)
    | .FLt(flt_type)
    | .FGt(flt_type)
    | .FLe(flt_type)
    | .FGe(flt_type)
    | .ExtendSI32
    | .ExtendUI32
    | .WrapI64
    | .TruncSF32
    | .TruncUF32
    | .TruncSF64
    | .TruncUF64
    | .TruncSatSF32
    | .TruncSatUF32
    | .TruncSatSF64
    | .TruncSatUF64
    | .ReinterpretFloat
    | .ReinterpretInt
    | .ConvertSI32
    | .ConvertUI32
    | .ConvertSI64
    | .ConvertUI64
    | .PromoteF32
    | .DemoteF64
    | .ReinterpretInt
  .

  public implementation display[wOp] => {
    disp(I) => d_instr(I,"")
  }

  d_instr:(wOp,string)=>string.
  d_instr(In,Off) => case In in {
    .Unreachable => "unreachable"
    | .Nop => "nop"
    | .Drop => "drop"
    | .Select => "select"
    | .Block(Lbl,Tp,Ins) => "#(disp_lbl(Lbl))block $(Tp) #(disp_ins(Ins,Off++"  ")) end"
    | .Loop(Lbl,Tp, Ins) => "#(disp_lbl(Lbl))loop $(Tp) #(disp_ins(Ins,Off++"  ")) end"
    | .If(Lbl,Tp,I,E) => "#(disp_lbl(Lbl))if $(Tp) #(disp_ins(I,Off++"  ")) else #(disp_ins(E,Off++"  ")) end"
    | .Br(Lbl) => "br $(Lbl)"
    | .BrIf(Lbl) => "br_if $(Lbl)"
    | .BrTable(Lbs,Dflt) => "br_table $(Lbs) $(Dflt)"
    | .Return => "return"
    | .Call(V) => "call $(V)"
    | .CallIndirect(X, Y) => "call_indirect $(X) $(Y)"
    | .LocalGet(V) => "local.get $(V)"
    | .LocalSet(V) => "local.set $(V)"
    | .LocalTee(V) => "local.tee $(V)"
    | .GlobalGet(V) => "global.get $(V)"
    | .GlobalSet(V) => "global.set $(V)"
    | .TableGet(V) => "table.get $(V)"
    | .TableSet(V) => "table.set $(V)"
    | .TableSize(V) => "table.size $(V)"
    | .TableGrow(V) => "table.grow $(V)"
    | .TableFill(V) => "table.fill $(V)"
    | .TableCopy(X, Y) => "table.copy $(X) $(Y)"
    | .TableInit(X, Y) => "table.init $(X) $(Y)"
    | .Load(L) => "load $(L)"
    | .Store(S) => "store $(S)"
    | .MemorySize => "memory.size"
    | .MemoryGrow => "memory.grow"
    | .MemoryFill => "memory.fill"
    | .MemoryCopy => "memory.copy"
    | .MemoryInit(V) => "memory.init"
    | .RefNull(T) => "ref.null $(T)"
    | .RefFunc(F) => "ref.func $(F)"
    | .RefIsNull => "ref.isnull"
    | .Const(N) => "$(N).const"
    | .IClz => "clz"
    | .ICtz => "ctz"
    | .Popcnt => "popcnt"
    | .IAdd(Tp) => "$(Tp).add"
    | .ISub(Tp) => "$(Tp).sub"
    | .IMul(Tp) => "$(Tp).mul"
    | .IDivS(Tp) => "$(Tp).divs"
    | .IDivU(Tp) => "$(Tp).divu"
    | .IRemS(Tp) => "$(Tp).rems"
    | .IRemU(Tp) => "$(Tp).remu"
    | .IAnd(Tp) => "$(Tp).and"
    | .IOr(Tp) => "$(Tp).or"
    | .IXor(Tp) => "$(Tp).xor"
    | .IShl(Tp) => "$(Tp).shl"
    | .IShrS(Tp) => "$(Tp).shrs"
    | .IShrU(Tp) => "$(Tp).shru"
    | .IRotl(Tp) => "$(Tp).rotl"
    | .IRotr(Tp) => "$(Tp).rotr"
    | .IEqz(Tp) => "$(Tp).eqz"
    | .IEq(Tp) => "$(Tp).eq"
    | .INe(Tp) => "$(Tp).ne"
    | .ILtS(Tp) => "$(Tp).lts"
    | .ILtU(Tp) => "$(Tp).ltu"
    | .IGtS(Tp) => "$(Tp).gts"
    | .IGtU(Tp) => "$(Tp).gtu"
    | .ILeS(Tp) => "$(Tp).les"
    | .ILeU(Tp) => "$(Tp).leu"
    | .IGeS(Tp) => "$(Tp).ges"
    | .IGeU(Tp) => "$(Tp).geu"
    | .FNeg(Tp) => "$(Tp).neg"
    | .FAbs(Tp) => "$(Tp).abs"
    | .FCeil(Tp) => "$(Tp).ceil"
    | .Floor(Tp) => "$(Tp).floor"
    | .FTrunc(Tp) => "$(Tp).trunc"
    | .FNearest(Tp) => "$(Tp).nearest"
    | .FSqrt(Tp) => "$(Tp).sqrt"
    | .FEq(Tp) => "$(Tp).eq"
    | .FNe(Tp) => "$(Tp).ne"
    | .FLt(Tp) => "$(Tp).lt"
    | .FGt(Tp) => "$(Tp).gt"
    | .FLe(Tp) => "$(Tp).le"
    | .FGe(Tp) => "$(Tp).ge"
    | .ExtendSI32 => "extendsi32"
    | .ExtendUI32 => "extendui32"
    | .WrapI64 => "i32.wrap_i64"
  }

  disp_ins:(cons[wOp],string)=>string.
  disp_ins(Ins,Off) => interleave(Ins//(I)=>d_instr(I,Off),"\n")*.

  public implementation display[wasmLbl] => {
    disp(.lbl(Lb)) => Lb.
  }

  disp_lbl(.none) => "".
  disp_lbl(.some(Lb)) => "$(Lb)\: ".

  public implementation display[pack_size] => {
    disp(.Pack8) => "p8".
    disp(.Pack16) => "p16".
    disp(.Pack32) => "p32".
    disp(.Pack64) => "p64"
  }
  
  public implementation display[extension] => {
    disp(.SX) => "sx".
    disp(.ZX) => "zx"
  }

  public implementation all t,p ~~ display[t], display[p] |: display[memop[t,p]] => {
    disp(memop{ty=T. align=A. offset=O. pack=P}) => "($(T) $(A) $(O) $(P))"
  }
}
