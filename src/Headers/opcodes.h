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
    XCall = 6,            // Call <prog>
    XOCall = 7,            // OCall
    XEscape = 8,            // call C escape
    TCall = 9,            // TCall <prog>
    TOCall = 10,            // TOCall
    Entry = 11,            // locals definition
    Ret = 12,            // return
    XRet = 13,            // return exception
    Block = 14,            // block of instructions
    Break = 15,            // leave block
    Result = 16,            // return value out of block
    Loop = 17,            // jump back to start of block
    Drop = 18,            // drop top of stack
    Dup = 19,            // duplicate top of stack
    Rot = 20,            // Pull up nth element of stack
    Rst = 21,            // reset stack height to a fixed height
    Pick = 22,            // adjust stack to n depth, using top k elements
    Fiber = 23,            // Create new fiber
    Suspend = 24,            // suspend fiber
    Resume = 25,            // resume fiber
    Retire = 26,            // retire a fiber
    Underflow = 27,            // underflow from current stack
    LdV = 28,            // Place a void value on stack
    LdC = 29,            // load literal from constant pool
    LdA = 30,            // load stack from args[xx]
    LdL = 31,            // load stack from local[xx]
    StL = 32,            // store tos to local[xx]
    StV = 33,            // clear a local to void
    TL = 34,            // copy tos to local[xx]
    LdG = 35,            // load a global variable
    StG = 36,            // store into a global variable
    TG = 37,            // copy into a global variable
    Sav = 38,            // create a single assignment variable
    LdSav = 39,            // derefence a sav, break if not set
    TstSav = 40,            // test a sav, return a logical
    StSav = 41,            // store a value into a single assignment variable
    TSav = 42,            // update single assignment variable leave value on stack
    Cell = 43,            // create R/W cell
    Get = 44,            // access a R/W cell
    Assign = 45,            // assign to a R/W cell
    CLbl = 46,            // T,Lbl --> test for a data term, break if not lbl
    CInt = 47,            // T,lit --> test for a literal integer, break if not
    CChar = 48,            // T,lit --> test for a literal char, break if not
    CFlt = 49,            // T,lit --> test for a literal floating point, break if not
    CLit = 50,            // T,lit --> test for a literal value, break if not
    Nth = 51,            // T --> el, pick up the nth element
    StNth = 52,            // T el --> store in nth element
    If = 53,            // break if true
    IfNot = 54,            // break if false
    ICase = 55,            // T --> T, icase <Max>
    Case = 56,            // T --> T, case <Max>
    IndxJmp = 57,            // check and jump on index
    IAdd = 58,            // L R --> L+R
    ISub = 59,            // L R --> L-R
    IMul = 60,            // L R --> L*R
    IDiv = 61,            // L R --> L/R
    IMod = 62,            // L R --> L%R
    IAbs = 63,            // L --> abs(L)
    IEq = 64,            // L R --> L==R
    ILt = 65,            // L R --> L<R
    IGe = 66,            // L R --> L>=R
    ICmp = 67,            // L R --> break if not same integer
    CEq = 68,            // L R --> L==R
    CLt = 69,            // L R --> L<R
    CGe = 70,            // L R --> L>=R
    CCmp = 71,            // L R --> break if not same character
    BAnd = 72,            // L R --> L&R
    BOr = 73,            // L R --> L|R
    BXor = 74,            // L R --> L^R
    BLsl = 75,            // L R --> L<<R
    BLsr = 76,            // L R --> L>>R
    BAsr = 77,            // L R --> L>>>R
    BNot = 78,            // L --> ~L
    FAdd = 79,            // L R --> L+R
    FSub = 80,            // L R --> L-R
    FMul = 81,            // L R --> L*R
    FDiv = 82,            // L R --> L/R
    FMod = 83,            // L R --> L%R
    FAbs = 84,            // L --> abs(L)
    FEq = 85,            // L R e --> L==R
    FLt = 86,            // L R --> L<R
    FGe = 87,            // L R --> L>=R
    FCmp = 88,            // L R --> branch if not same floating point
    Alloc = 89,            // new structure, elements from stack
    Closure = 90,            // allocate a closure
    Cmp = 91,            // t1 t2 --> , branch to offset if not same literal
    Frame = 92,            // frame instruction
    dBug = 93,            // debugging prefix

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
      "XCall",
      "XOCall",
      "XEscape",
      "TCall",
      "TOCall",
      "Entry",
      "Ret",
      "XRet",
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
      "LdV",
      "LdC",
      "LdA",
      "LdL",
      "StL",
      "StV",
      "TL",
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
      "CInt",
      "CChar",
      "CFlt",
      "CLit",
      "Nth",
      "StNth",
      "If",
      "IfNot",
      "ICase",
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
#define OPCODE_SIGNATURE 2231337229613573684
#endif

typedef enum {
  nOp,          // No operand
  tOs,          // top of stack
  i32,          /* 32 bit literal operand */
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
} opAndSpec;    // Specification code for an operand

#endif
