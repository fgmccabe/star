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
    Suspend = 20,            // suspend fiber
    Resume = 21,            // resume fiber
    Retire = 22,            // retire a fiber
    Underflow = 23,            // underflow from current stack
    Try = 24,            // a try-catch block
    EndTry = 25,            // end try
    TryRslt = 26,            // end try with a  result
    Throw = 27,            // Invoke a continuation
    LdV = 28,            // Place a void value on stack
    LdC = 29,            // load literal from constant pool
    LdA = 30,            // load stack from args[xx]
    LdL = 31,            // load stack from local[xx]
    StL = 32,            // store tos to local[xx]
    StV = 33,            // clear a local to void
    TL = 34,            // copy tos to local[xx]
    LdS = 35,            // lift a value from the stack
    LdG = 36,            // load a global variable
    StG = 37,            // store into a global variable
    TG = 38,            // copy into a global variable
    Sav = 39,            // create a single assignment variable
    LdSav = 40,            // derefence a sav, break if not set
    TstSav = 41,            // test a sav, return a logical
    StSav = 42,            // store a value into a single assignment variable
    TSav = 43,            // update single assignment variable leave value on stack
    Cell = 44,            // create R/W cell
    Get = 45,            // access a R/W cell
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
      "Suspend",
      "Resume",
      "Retire",
      "Underflow",
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
#define OPCODE_SIGNATURE 556212363986565336
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
