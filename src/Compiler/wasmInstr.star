star.compiler.wasm.instr{
  import star.
  import star.multi.
  import star.compiler.wasm.types.

  var ~> string.

  public idx ~> integer.

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

  vec_loadop ~> memop[vec_type,option[(pack_size,extension)]].
  vec_storeop ~> memop[vec_type,()].
  vec_laneop ~> (memop[vec_type,pack_size],integer).

  public pack_size ::= .Pack8 | .Pack16 | .Pack32 | .Pack64.
  public extension ::= .SX | .ZX.

  public initop ::= .Explicit | .Implicit.
  public externop ::= .Internalize | .Externalize.

  public wOp ::= 
    .Unreachable                       --  trap unconditionally
  | .Nop			       --  do nothing
  | .Drop			       --  forget a value
  | .Select(option[cons[value_type]])  --  branchless conditional
  | .Block(block_type, cons[wOp])      --  execute in sequence
  | .Loop(block_type, cons[wOp])	    --  loop header
  | .If(block_type, cons[wOp], cons[wOp]) --  conditional
  | .Br(idx)			       --  break to n-th surrounding label
  | .BrIf(idx)		               --  conditional break
  | .BrTable(cons[idx], idx)           --  indexed break
  | .BrOnNull(idx)                     --  break on type
  | .BrOnNonNull(idx)                  --  inverted break on type
  | .BrOnCast(idx,ref_type,ref_type)   --  break on type
  | .BrOnCastFail(idx,ref_type,ref_type)   --  break on type
  | .Return			       --  break from function body
  | .Call(idx)		               --  call function
  | .CallRef(idx)		       --  call function reference
  | .CallIndirect(idx, idx)	       --  call function through table
  | .ReturnCall(idx)		       --  tail call function
  | .ReturnCallRef(idx)		       --  tail call function
  | .ReturnCallIndirect(idx,idx)       --  tail call function through table
  | .LocalGet(idx)		       --  read local variable
  | .LocalSet(idx)		       --  write local variable
  | .LocalTee(idx)		       --  write local variable and keep value
  | .GlobalGet(idx)		       --  read global variable
  | .GlobalSet(idx)		       --  write global variable
  | .TableGet(idx)		       --  read table element
  | .TableSet(idx)		       --  write table element
  | .TableSize(idx)		       --  size of table
  | .TableGrow(idx)		       --  grow table
  | .TableFill(idx)		       --  fill table range with value
  | .TableCopy(idx, idx)	       --  copy table range
  | .TableInit(idx, idx)	       --  initialize table range from segment
  | .ElemDrop(idx)		       --  drop passive element segment
  | .Load(loadop)		       --  read memory at address
  | .Store(storeop)		       --  write memory at address
  | .VecLoad(vec_loadop)               --  read memory at address
  | .VecStore(vec_storeop)             --  write memory at address
  | .VecLoadLane(vec_laneop)           --  read single line at address
  | .VecStoreLane(vec_laneop)          --  write single line to address
  | .MemorySize			       --  size of memory
  | .MemoryGrow			       --  grow memory
  | .MemoryFill			       --  fill memory range with value
  | .MemoryCopy			       --  copy memory ranges
  | .MemoryInit(idx)		       --  initialize memory range from segment
  | .DataDrop(idx)		       --  drop passive data segment
  | .IConst(int_type,integer)	       --  integer constant
  | .FConst(flt_type,float)	       --  floating point constant
  | .RefNull(ref_type)		       --  null reference
  | .RefFunc(idx)		       --  function reference
  | .RefIsNull			       --  null test
  | .RefAsNotNull		       --  null test
  | .RefAsNotNull		       --  null test
  | .RefTest(ref_type)                 --  type test
  | .RefCast(ref_type)                 --  type cast
  | .RefEq                             --  reference equality
  | .RefI31                            --  scalar reference
  | .I31Get(extension)                 --  read scalar
  | .StructNew(idx,initop)             --  allocate structure
  | .StructGet(idx,idx,option[extension]) -- Access element of structure
  | .StructSet(idx,idx)
  | .ArrayNew(idx,initop)              --  allocate array
  | .ArrayNewFixed(idx,integer)        --  allocate fixed array
  | .ArrayNewElem(idx,idx)             --  allocate array from elem segment
  | .ArrayNewData(idx,idx)             --  allocate array from data segment
  | .ArrayGet(idx,option[extension])   --  access array slot
  | .ArraySet(idx)                     --  store array slot
  | .ArrayLen                          --  length of array
  | .ArrayCopy(idx,idx)                --  copy between two arrays
  | .ArrayFill(idx)                    --  fill array with value
  | .ArrayInitData(idx,idx)            --  initialize array from data segment
  | .ArrayInitElem(idx,idx)            --  initialize array from elem segment
  | .ExternConvert(externop)
--  | .VecConst(vec)                     --  contant
--  | .VecTest(vec_testop)               --  vector test
--  | .VecCompare(vec_relop)             --  vector compare
--  | .VecUnary(vec_unop)                --  vector unary operator
--  | .VecBinary(vec_binop)              --  vector binary operator
--  | .VecConvert(vec_cvtop)             --  vector conversion
--  | .VecShift(vec_shiftop)             --  vector shift
--  | .VecBitmask(vec_bitmaskop)         --  vector masking
--  | .VecTestBits(vec_vtestop)          --  vector bit test
--  | .VecUnaryBits(vec_vunop)           --  unary bit vector operator
--  | .VecBinaryBits(vec_vbinop)         --  binary bit vector
--  | .VecTernaryBits(vec_vternop)       --  ternary bit vector operator
--  | .VecSplat(vec_splatop)             --  number to vector conversion
--  | .VecExtract(vec_extractop)         --  extract lane from vector
--  | .VecReplace(vec_replaceop)         --  replace lane in vector
  | .i31Ref			       --  Convert i32 to i31Ref
  | .IEqz(int_type)		       -- equal to zero
  | .IEq(int_type)		       -- Integer comparisons
  | .INe(int_type)
  | .ILtS(int_type)
  | .ILtU(int_type)
  | .IGtS(int_type)
  | .IGtU(int_type)
  | .ILeS(int_type)
  | .ILeU(int_type)
  | .IGeS(int_type)
  | .IGeU(int_type)
  | .IClz(int_type)                     -- unary integer operations
  | .ICtz(int_type)
  | .IPopcnt(int_type)
  | .IAdd(int_type)                     -- binary arithmetic operations
  | .ISub(int_type)                     -- integer subtraction
  | .IMul(int_type)                     -- integer multiplication
  | .IDivS(int_type)                    -- integer signed division
  | .IDivU(int_type)                    -- integer unsigned division
  | .IRemS(int_type)                    -- integer signed remainder
  | .IRemU(int_type)                    -- integer unsigned remainder
  | .IAnd(int_type)                     -- integer bitwise and
  | .IOr(int_type)                      -- integer bitwise or
  | .IXor(int_type)                     -- integer bitwise exclusive or
  | .IShl(int_type)                     -- integer bitwise shift left
  | .IShrS(int_type)                    -- integer bitwise arithmetic shift right
  | .IShrU(int_type)                    -- integer bitwise logical shift right
  | .IRotl(int_type)                    -- integer bitwise rotate left
  | .IRotr(int_type)                    -- integer bitwise rotate right
  | .FNeg(flt_type)			-- Unary floating point
  | .FAbs(flt_type)
  | .FCeil(flt_type)
  | .Floor(flt_type)
  | .FTrunc(flt_type)
  | .FNearest(flt_type)
  | .FSqrt(flt_type)
  | .FEq(flt_type)			-- Binary floating point
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
    | .Unreachable => "unreachable"
    | .Nop => "nop"
    | .Drop => "drop"
    | .Select(.none) => "select"
    | .Select(.some(Tps)) => "select $(Tps)"
    | .Block(Tp,Ins) => "block $(Tp) #(disp_ins(Ins,Off++"  ")) end"
    | .Loop(Tp, Ins) => "loop $(Tp) #(disp_ins(Ins,Off++"  ")) end"
    | .If(Tp,I,E) => "if $(Tp) #(disp_ins(I,Off++"  ")) else #(disp_ins(E,Off++"  ")) end"
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
    | .IConst(_,N) => "$(N).const"
    | .FConst(_,N) => "$(N).const"
    | .IClz(Tp) => "$(Tp).clz"
    | .ICtz(Tp) => "$(Tp).ctz"
    | .IPopcnt(Tp) => "$(Tp).popcnt"
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
    | .i31Ref => "ref.i31"
  }

  disp_ins:(cons[wOp],string)=>string.
  disp_ins(Ins,Off) => interleave(Ins//(I)=>d_instr(I,Off),"\n")*.

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
