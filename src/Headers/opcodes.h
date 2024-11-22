/* 
 * Access the opcodes of the Star machine
 * (c) 2013, 2018, 2024 and beyond F.G.McCabe
 * all rights reserved
 **/

#ifndef OPCODES_H_
#define OPCODES_H_

typedef enum {
    Halt = 0,            // Stop execution
    Nop = 1,            // No operation
    Abort = 2,            // abort with message
    Call = 3,            // Call <prog>
    OCall = 4,            // OCall
    Escape = 5,            // call C escape
    TCall = 6,            // TCall <prog>
    TOCall = 7,            // TOCall
    Entry = 8,            // locals definition
    Ret = 9,            // return
    Block = 10,            // block of instructions
    Break = 11,            // leave block
    Loop = 12,            // jump back to start of block
    Drop = 13,            // drop top of stack
    Dup = 14,            // duplicate top of stack
    Rot = 15,            // Pull up nth element of stack
    Rst = 16,            // reset stack height to a fixed height
    Fiber = 17,            // Create new fiber
    Spawn = 18,            // spawn a new task
    Suspend = 19,            // suspend fiber
    Resume = 20,            // resume fiber
    Retire = 21,            // retire a fiber
    Underflow = 22,            // underflow from current stack
    Try = 23,            // a try-catch block
    EndTry = 24,            // end try
    Throw = 25,            // Invoke a continuation
    Reset = 26,            // establish a delimited zone
    Shift = 27,            // capture continuation
    Invoke = 28,            // invoke continuation
    LdV = 29,            // Place a void value on stack
    LdC = 30,            // load literal from constant pool
    LdA = 31,            // load stack from args[xx]
    LdL = 32,            // load stack from local[xx]
    StL = 33,            // store tos to local[xx]
    StV = 34,            // clear a local to void
    TL = 35,            // copy tos to local[xx]
    LdS = 36,            // lift a value from the stack
    LdG = 37,            // load a global variable
    StG = 38,            // store into a global variable
    TG = 39,            // copy into a global variable
    Thunk = 40,            // create a thunk from a lambda
    LdTh = 41,            // derefence a thunk, potentially running its lambda
    StTh = 42,            // store a value into a thunk variable
    TTh = 43,            // update thunk and leave on stack
    Cell = 44,            // create R/W cell
    Get = 45,            // access a R/W cell, break if not set
    Assign = 46,            // assign to a R/W cell
    CLbl = 47,            // T,Lbl --> test for a data term, break if not lbl
    CLit = 48,            // T,lit --> test for a literal value, break if not
    Nth = 49,            // T --> el, pick up the nth element
    StNth = 50,            // T el --> store in nth element
    If = 51,            // break if true
    IfNot = 52,            // break if false
    Case = 53,            // T --> T, case <Max> 
    IndxJmp = 54,            // check and jump on index
    IAdd = 55,            // L R --> L+R
    ISub = 56,            // L R --> L-R
    IMul = 57,            // L R --> L*R
    IDiv = 58,            // L R --> L/R
    IMod = 59,            // L R --> L%R
    IAbs = 60,            // L --> abs(L)
    IEq = 61,            // L R --> L==R
    ILt = 62,            // L R --> L<R
    IGe = 63,            // L R --> L>=R
    ICmp = 64,            // L R --> break if not same integer
    CEq = 65,            // L R --> L==R
    CLt = 66,            // L R --> L<R
    CGe = 67,            // L R --> L>=R
    CCmp = 68,            // L R --> break if not same character
    BAnd = 69,            // L R --> L&R
    BOr = 70,            // L R --> L|R
    BXor = 71,            // L R --> L^R
    BLsl = 72,            // L R --> L<<R
    BLsr = 73,            // L R --> L>>R
    BAsr = 74,            // L R --> L>>>R
    BNot = 75,            // L --> ~L
    FAdd = 76,            // L R --> L+R
    FSub = 77,            // L R --> L-R
    FMul = 78,            // L R --> L*R
    FDiv = 79,            // L R --> L/R
    FMod = 80,            // L R --> L%R
    FAbs = 81,            // L --> abs(L)
    FEq = 82,            // L R e --> L==R
    FLt = 83,            // L R --> L<R
    FGe = 84,            // L R --> L>=R
    FCmp = 85,            // L R --> branch if not same floating point
    Alloc = 86,            // new structure, elements from stack
    Closure = 87,            // allocate a closure
    Cmp = 88,            // t1 t2 --> , branch to offset if not same literal
    Frame = 89,            // frame instruction
    dBug = 90,            // debugging prefix

  illegalOp,
  maxOpCode
} OpCode;

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 544731968861105627
#endif

typedef enum {
  nOp,                                   // No operand
  tOs,          // top of stack
  i32,         /* 32 bit literal operand */
  art,          /* Arity */
  arg,          /* argument variable offset */
  lcl,          /* local variable offset */
  lcs,          // Store to local variable
  Es,           // escape code 0..65535
  lit,          /* constant literal */
  sym,          // Symbol
  glb,          // Global variable name
  tPe,          // Type signature
  bLk,          // A block of instructions
  lVl,          // How many blocks to break out of
} opAndSpec;                    // Specification code for an operand

#endif
