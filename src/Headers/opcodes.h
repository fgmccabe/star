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
    Result = 12,            // return value out of block
    Loop = 13,            // jump back to start of block
    Drop = 14,            // drop top of stack
    Dup = 15,            // duplicate top of stack
    Rot = 16,            // Pull up nth element of stack
    Rst = 17,            // reset stack height to a fixed height
    Pick = 18,            // adjust stack to n depth, using top k elements
    Fiber = 19,            // Create new fiber
    Spawn = 20,            // spawn a new task
    Suspend = 21,            // suspend fiber
    Resume = 22,            // resume fiber
    Retire = 23,            // retire a fiber
    Underflow = 24,            // underflow from current stack
    VoidTry = 25,            // generate a void handler code
    Try = 26,            // a try-catch block
    EndTry = 27,            // end try
    TryRslt = 28,            // end try with a  result
    Throw = 29,            // Invoke a continuation
    LdV = 30,            // Place a void value on stack
    LdC = 31,            // load literal from constant pool
    LdA = 32,            // load stack from args[xx]
    LdL = 33,            // load stack from local[xx]
    StL = 34,            // store tos to local[xx]
    StV = 35,            // clear a local to void
    TL = 36,            // copy tos to local[xx]
    LdS = 37,            // lift a value from the stack
    LdG = 38,            // load a global variable
    StG = 39,            // store into a global variable
    TG = 40,            // copy into a global variable
    Sav = 41,            // create a single assignment variable
    LdSav = 42,            // derefence a sav, break if not set
    TstSav = 43,            // test a sav, return a logical
    StSav = 44,            // store a value into a single assignment variable
    TSav = 45,            // update single assignment variable leave value on stack
    Cell = 46,            // create R/W cell
    Get = 47,            // access a R/W cell
    Assign = 48,            // assign to a R/W cell
    CLbl = 49,            // T,Lbl --> test for a data term, break if not lbl
    CLit = 50,            // T,lit --> test for a literal value, break if not
    Nth = 51,            // T --> el, pick up the nth element
    StNth = 52,            // T el --> store in nth element
    If = 53,            // break if true
    IfNot = 54,            // break if false
    Case = 55,            // T --> T, case <Max>
    IndxJmp = 56,            // check and jump on index
    IAdd = 57,            // L R --> L+R
    ISub = 58,            // L R --> L-R
    IMul = 59,            // L R --> L*R
    IDiv = 60,            // L R --> L/R
    IMod = 61,            // L R --> L%R
    IAbs = 62,            // L --> abs(L)
    IEq = 63,            // L R --> L==R
    ILt = 64,            // L R --> L<R
    IGe = 65,            // L R --> L>=R
    ICmp = 66,            // L R --> break if not same integer
    CEq = 67,            // L R --> L==R
    CLt = 68,            // L R --> L<R
    CGe = 69,            // L R --> L>=R
    CCmp = 70,            // L R --> break if not same character
    BAnd = 71,            // L R --> L&R
    BOr = 72,            // L R --> L|R
    BXor = 73,            // L R --> L^R
    BLsl = 74,            // L R --> L<<R
    BLsr = 75,            // L R --> L>>R
    BAsr = 76,            // L R --> L>>>R
    BNot = 77,            // L --> ~L
    FAdd = 78,            // L R --> L+R
    FSub = 79,            // L R --> L-R
    FMul = 80,            // L R --> L*R
    FDiv = 81,            // L R --> L/R
    FMod = 82,            // L R --> L%R
    FAbs = 83,            // L --> abs(L)
    FEq = 84,            // L R e --> L==R
    FLt = 85,            // L R --> L<R
    FGe = 86,            // L R --> L>=R
    FCmp = 87,            // L R --> branch if not same floating point
    Alloc = 88,            // new structure, elements from stack
    Closure = 89,            // allocate a closure
    Cmp = 90,            // t1 t2 --> , branch to offset if not same literal
    Frame = 91,            // frame instruction
    dBug = 92,            // debugging prefix

  illegalOp,
  maxOpCode
} OpCode;

#ifndef NDEBUG
static char *opNames[] = {
      "Halt",
      "Nop",
      "Abort",
      "Call",
      "OCall",
      "Escape",
      "TCall",
      "TOCall",
      "Entry",
      "Ret",
      "Block",
      "Break",
      "Result",
      "Loop",
      "Drop",
      "Dup",
      "Rot",
      "Rst",
      "Pick",
      "Fiber",
      "Spawn",
      "Suspend",
      "Resume",
      "Retire",
      "Underflow",
      "VoidTry",
      "Try",
      "EndTry",
      "TryRslt",
      "Throw",
      "LdV",
      "LdC",
      "LdA",
      "LdL",
      "StL",
      "StV",
      "TL",
      "LdS",
      "LdG",
      "StG",
      "TG",
      "Sav",
      "LdSav",
      "TstSav",
      "StSav",
      "TSav",
      "Cell",
      "Get",
      "Assign",
      "CLbl",
      "CLit",
      "Nth",
      "StNth",
      "If",
      "IfNot",
      "Case",
      "IndxJmp",
      "IAdd",
      "ISub",
      "IMul",
      "IDiv",
      "IMod",
      "IAbs",
      "IEq",
      "ILt",
      "IGe",
      "ICmp",
      "CEq",
      "CLt",
      "CGe",
      "CCmp",
      "BAnd",
      "BOr",
      "BXor",
      "BLsl",
      "BLsr",
      "BAsr",
      "BNot",
      "FAdd",
      "FSub",
      "FMul",
      "FDiv",
      "FMod",
      "FAbs",
      "FEq",
      "FLt",
      "FGe",
      "FCmp",
      "Alloc",
      "Closure",
      "Cmp",
      "Frame",
      "dBug"};
#endif

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 1642028862004111901
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
