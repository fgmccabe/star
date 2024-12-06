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
    LdV = 26,            // Place a void value on stack
    LdC = 27,            // load literal from constant pool
    LdA = 28,            // load stack from args[xx]
    LdL = 29,            // load stack from local[xx]
    StL = 30,            // store tos to local[xx]
    StV = 31,            // clear a local to void
    TL = 32,            // copy tos to local[xx]
    LdS = 33,            // lift a value from the stack
    LdG = 34,            // load a global variable
    StG = 35,            // store into a global variable
    TG = 36,            // copy into a global variable
    Thunk = 37,            // create a thunk from a lambda
    LdTh = 38,            // derefence a thunk, potentially running its lambda
    StTh = 39,            // store a value into a thunk variable
    TTh = 40,            // update thunk and leave on stack
    Sav = 41,            // create a single assignment variable
    LdSav = 42,            // derefence a sav, break if not set
    StSav = 43,            // store a value into a single assignment variable
    TSav = 44,            // update single assignment variable leave value on stack
    Cell = 45,            // create R/W cell
    Get = 46,            // access a R/W cell
    Assign = 47,            // assign to a R/W cell
    CLbl = 48,            // T,Lbl --> test for a data term, break if not lbl
    CLit = 49,            // T,lit --> test for a literal value, break if not
    Nth = 50,            // T --> el, pick up the nth element
    StNth = 51,            // T el --> store in nth element
    If = 52,            // break if true
    IfNot = 53,            // break if false
    Case = 54,            // T --> T, case <Max> 
    IndxJmp = 55,            // check and jump on index
    IAdd = 56,            // L R --> L+R
    ISub = 57,            // L R --> L-R
    IMul = 58,            // L R --> L*R
    IDiv = 59,            // L R --> L/R
    IMod = 60,            // L R --> L%R
    IAbs = 61,            // L --> abs(L)
    IEq = 62,            // L R --> L==R
    ILt = 63,            // L R --> L<R
    IGe = 64,            // L R --> L>=R
    ICmp = 65,            // L R --> break if not same integer
    CEq = 66,            // L R --> L==R
    CLt = 67,            // L R --> L<R
    CGe = 68,            // L R --> L>=R
    CCmp = 69,            // L R --> break if not same character
    BAnd = 70,            // L R --> L&R
    BOr = 71,            // L R --> L|R
    BXor = 72,            // L R --> L^R
    BLsl = 73,            // L R --> L<<R
    BLsr = 74,            // L R --> L>>R
    BAsr = 75,            // L R --> L>>>R
    BNot = 76,            // L --> ~L
    FAdd = 77,            // L R --> L+R
    FSub = 78,            // L R --> L-R
    FMul = 79,            // L R --> L*R
    FDiv = 80,            // L R --> L/R
    FMod = 81,            // L R --> L%R
    FAbs = 82,            // L --> abs(L)
    FEq = 83,            // L R e --> L==R
    FLt = 84,            // L R --> L<R
    FGe = 85,            // L R --> L>=R
    FCmp = 86,            // L R --> branch if not same floating point
    Alloc = 87,            // new structure, elements from stack
    Closure = 88,            // allocate a closure
    Cmp = 89,            // t1 t2 --> , branch to offset if not same literal
    Frame = 90,            // frame instruction
    dBug = 91,            // debugging prefix

  illegalOp,
  maxOpCode
} OpCode;

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 1236055311775057726
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
