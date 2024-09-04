/* 
 * Access the opcodes of the Star machine
 * (c) 2013, 2018, 2024 and beyond F.G.McCabe
 * all rights reserved
 **/

#ifndef _OPCODES_H_
#define _OPCODES_H_

typedef enum {
      Halt = 0,            // Stop execution
    Nop = 1,            // No operation
    Abort = 2,            // abort with message
    Call = 3,            // Call <prog>
    OCall = 4,            // OCall
    Escape = 5,            // call C escape
    TCall = 6,            // TCall <prog>
    TOCall = 7,            // TOCall
    Locals = 8,            // locals definition
    Ret = 9,            // return
    Jmp = 10,            // jump lbl
    Drop = 11,            // drop top of stack
    Dup = 12,            // duplicate top of stack
    Rot = 13,            // Pull up nth element of stack
    Rst = 14,            // reset stack height to a fixed height
    Fiber = 15,            // Create new fiber
    Spawn = 16,            // spawn a new task
    Suspend = 17,            // suspend fiber
    Resume = 18,            // resume fiber
    Retire = 19,            // retire a fiber
    Underflow = 20,            // underflow from current stack
    TEq = 21,            // L R --> L==R, where L,R are tasks
    Try = 22,            // start a try-catch block
    EndTry = 23,            // clear a try block
    Throw = 24,            // Invoke a continuation
    Invoke = 25,            // invoke continuation
    LdV = 26,            // Place a void value on stack
    LdC = 27,            // load literal from constant pool
    LdA = 28,            // load stack from args[xx]
    LdL = 29,            // load stack from local[xx]
    StL = 30,            // store tos to local[xx]
    StV = 31,            // clear a local to void
    TL = 32,            // copy tos to local[xx]
    StA = 33,            // store tos to args[xx]
    LdG = 34,            // load a global variable
    StG = 35,            // store into a global variable
    TG = 36,            // copy into a global variable
    Thunk = 37,            // create a thunk from a lambda
    LdTh = 38,            // derefence a thunk, potentially running its lambda
    StTh = 39,            // store a value into a thunk variable
    TTh = 40,            // update thunk and leave on stack
    Cell = 41,            // create R/W cell
    Get = 42,            // access a R/W cell
    Assign = 43,            // assign to a R/W cell
    CLbl = 44,            // T,Lbl --> test for a data term, branch if lbl
    Nth = 45,            // T --> el, pick up the nth element
    StNth = 46,            // T el --> store in nth element
    If = 47,            // break if true
    IfNot = 48,            // break if false
    Case = 49,            // T --> T, case <Max> 
    IndxJmp = 50,            // check and jump on index
    Unpack = 51,            // check against term & unpack
    IAdd = 52,            // L R --> L+R
    ISub = 53,            // L R --> L-R
    IMul = 54,            // L R --> L*R
    IDiv = 55,            // L R --> L/R
    IMod = 56,            // L R --> L%R
    IAbs = 57,            // L --> abs(L)
    IEq = 58,            // L R --> L==R
    ILt = 59,            // L R --> L<R
    IGe = 60,            // L R --> L>=R
    ICmp = 61,            // L R --> break if not same integer
    CEq = 62,            // L R --> L==R
    CLt = 63,            // L R --> L<R
    CGe = 64,            // L R --> L>=R
    CCmp = 65,            // L R --> break if not same character
    BAnd = 66,            // L R --> L&R
    BOr = 67,            // L R --> L|R
    BXor = 68,            // L R --> L^R
    BLsl = 69,            // L R --> L<<R
    BLsr = 70,            // L R --> L>>R
    BAsr = 71,            // L R --> L>>>R
    BNot = 72,            // L --> ~L
    FAdd = 73,            // L R --> L+R
    FSub = 74,            // L R --> L-R
    FMul = 75,            // L R --> L*R
    FDiv = 76,            // L R --> L/R
    FMod = 77,            // L R --> L%R
    FAbs = 78,            // L --> abs(L)
    FEq = 79,            // L R e --> L==R
    FLt = 80,            // L R --> L<R
    FGe = 81,            // L R --> L>=R
    FCmp = 82,            // L R --> branch if not same floating point
    Alloc = 83,            // new structure, elements from stack
    Closure = 84,            // allocate a closure
    Cmp = 85,            // t1 t2 --> , branch to offset if not same literal
    Frame = 86,            // frame instruction
    dBug = 87,            // debugging prefix

  label,
  illegalOp,
  maxOpCode
} OpCode;

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 762209321258706293
#endif

typedef enum {
  nOp,                                   // No operand
  tOs,          // top of stack
  i32,         /* 32 bit literal operand */
  art,          /* Arity */
  arg,          /* argument variable offset */
  lcl,          /* local variable offset */
  lcs,          // Store to local variable
  off,          /* offset within current code */
  Es,           // escape code 0..65535
  lit,          /* constant literal */
  sym,          // Symbol
  glb,          // Global variable name
  tPe,          // Type signature
  bLk,          // A block of instructions
  lVl,          // How many blocks to break out of
} opAndSpec;                    // Specification code for an operand

#endif

