/* 
 * Access the opcodes of the Star machine
 * (c) 2013, 2018, 2024 and beyond F.G.McCabe
 * all rights reserved
 **/

#ifndef OPCODES_H_
#define OPCODES_H_

typedef enum {
    Halt = 0,            // Stop execution
    Abort = 1,            // abort with message
    Call = 2,            // Call <prog>
    OCall = 3,            // OCall
    Escape = 4,            // call C escape
    XCall = 5,            // Call <prog>
    XOCall = 6,            // OCall
    XEscape = 7,            // call C escape
    TCall = 8,            // TCall <prog>
    TOCall = 9,            // TOCall
    Entry = 10,            // locals definition
    Ret = 11,            // return
    XRet = 12,            // return exception
    Block = 13,            // block of instructions
    Valof = 14,            // return value from block
    Break = 15,            // leave block
    Result = 16,            // return value out of block
    Loop = 17,            // jump back to start of block
    Drop = 18,            // drop top of stack
    Dup = 19,            // duplicate top of stack
    Rot = 20,            // Pull up nth element of stack
    Rst = 21,            // reset stack height to a fixed height
    Fiber = 22,            // Create new fiber
    Suspend = 23,            // suspend fiber
    Resume = 24,            // resume fiber
    Retire = 25,            // retire a fiber
    Underflow = 26,            // underflow from current stack
    LdV = 27,            // Place a void value on stack
    LdC = 28,            // load literal from constant pool
    LdA = 29,            // load stack from args[xx]
    LdL = 30,            // load stack from local[xx]
    StL = 31,            // store tos to local[xx]
    StV = 32,            // clear a local to void
    TL = 33,            // copy tos to local[xx]
    LdG = 34,            // load a global variable
    StG = 35,            // store into a global variable
    TG = 36,            // copy into a global variable
    Sav = 37,            // create a single assignment variable
    LdSav = 38,            // derefence a sav, break if not set
    TstSav = 39,            // test a sav, return a logical
    StSav = 40,            // store a value into a single assignment variable
    TSav = 41,            // update single assignment variable leave value on stack
    Cell = 42,            // create R/W cell
    Get = 43,            // access a R/W cell
    Assign = 44,            // assign to a R/W cell
    CLbl = 45,            // T,Lbl --> test for a data term, break if not lbl
    CInt = 46,            // T,lit --> test for a literal integer, break if not
    CChar = 47,            // T,lit --> test for a literal char, break if not
    CFlt = 48,            // T,lit --> test for a literal floating point, break if not
    CLit = 49,            // T,lit --> test for a literal value, break if not
    Nth = 50,            // T --> el, pick up the nth element
    StNth = 51,            // T el --> store in nth element
    If = 52,            // break if true
    IfNot = 53,            // break if false
    ICase = 54,            // T --> T, icase <Max>
    Case = 55,            // T --> T, case <Max>
    IxCase = 56,            // check and jump on type index
    IAdd = 57,            // L R --> L+R
    ISub = 58,            // L R --> L-R
    IMul = 59,            // L R --> L*R
    IDiv = 60,            // L R --> L/R
    IMod = 61,            // L R --> L%R
    IAbs = 62,            // L --> abs(L)
    IEq = 63,            // L R --> L==R
    ILt = 64,            // L R --> L<R
    IGe = 65,            // L R --> L>=R
    CEq = 66,            // L R --> L==R
    CLt = 67,            // L R --> L<R
    CGe = 68,            // L R --> L>=R
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
    Alloc = 85,            // new structure, elements from stack
    Closure = 86,            // allocate a closure
    Frame = 87,            // frame instruction
    Line = 88,            // source line indicator
    dBug = 89,            // debugging prefix

  illegalOp,
  maxOpCode
} OpCode;

static char *opNames[] = {
      "Halt",
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
      "Valof",
      "Break",
      "Result",
      "Loop",
      "Drop",
      "Dup",
      "Rot",
      "Rst",
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
      "IxCase",
      "IAdd",
      "ISub",
      "IMul",
      "IDiv",
      "IMod",
      "IAbs",
      "IEq",
      "ILt",
      "IGe",
      "CEq",
      "CLt",
      "CGe",
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
      "Alloc",
      "Closure",
      "Frame",
      "Line",
      "dBug"};

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 1157978942296785718
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
