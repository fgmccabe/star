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
    Rot = 19,            // Pull up nth element of stack
    Rst = 20,            // reset stack height to a fixed height
    Fiber = 21,            // Create new fiber
    Suspend = 22,            // suspend fiber
    Resume = 23,            // resume fiber
    Retire = 24,            // retire a fiber
    Underflow = 25,            // underflow from current stack
    LdV = 26,            // Place a void value on stack
    LdC = 27,            // load literal from constant pool
    Ld = 28,            // load stack from local[xx]
    St = 29,            // store tos to local[xx]
    StV = 30,            // clear a local to void
    Tee = 31,            // copy tos to local[xx]
    LdG = 32,            // load a global variable
    StG = 33,            // store into a global variable
    TG = 34,            // copy into a global variable
    Sav = 35,            // create a single assignment variable
    LdSav = 36,            // derefence a sav, break if not set
    TstSav = 37,            // test a sav, return a logical
    StSav = 38,            // store a value into a single assignment variable
    TSav = 39,            // update single assignment variable leave value on stack
    Cell = 40,            // create R/W cell
    Get = 41,            // access a R/W cell
    Assign = 42,            // assign to a R/W cell
    CLbl = 43,            // T,Lbl --> test for a data term, break if not lbl
    CInt = 44,            // T,lit --> test for a literal integer, break if not
    CChar = 45,            // T,lit --> test for a literal char, break if not
    CFlt = 46,            // T,lit --> test for a literal floating point, break if not
    CLit = 47,            // T,lit --> test for a literal value, break if not
    Nth = 48,            // T --> el, pick up the nth element
    StNth = 49,            // T el --> store in nth element
    If = 50,            // break if true
    IfNot = 51,            // break if false
    ICase = 52,            // T --> T, icase <Max>
    Case = 53,            // T --> T, case <Max>
    IxCase = 54,            // check and jump on type index
    IAdd = 55,            // L R --> L+R
    ISub = 56,            // L R --> L-R
    IMul = 57,            // L R --> L*R
    IDiv = 58,            // L R --> L/R
    IMod = 59,            // L R --> L%R
    IAbs = 60,            // L --> abs(L)
    IEq = 61,            // L R --> L==R
    ILt = 62,            // L R --> L<R
    IGe = 63,            // L R --> L>=R
    CEq = 64,            // L R --> L==R
    CLt = 65,            // L R --> L<R
    CGe = 66,            // L R --> L>=R
    BAnd = 67,            // L R --> L&R
    BOr = 68,            // L R --> L|R
    BXor = 69,            // L R --> L^R
    BLsl = 70,            // L R --> L<<R
    BLsr = 71,            // L R --> L>>R
    BAsr = 72,            // L R --> L>>>R
    BNot = 73,            // L --> ~L
    FAdd = 74,            // L R --> L+R
    FSub = 75,            // L R --> L-R
    FMul = 76,            // L R --> L*R
    FDiv = 77,            // L R --> L/R
    FMod = 78,            // L R --> L%R
    FAbs = 79,            // L --> abs(L)
    FEq = 80,            // L R e --> L==R
    FLt = 81,            // L R --> L<R
    FGe = 82,            // L R --> L>=R
    Alloc = 83,            // new structure, elements from stack
    Closure = 84,            // allocate a closure
    Frame = 85,            // frame instruction
    Line = 86,            // source line indicator
    Bind = 87,            // bind a variable
    dBug = 88,            // debugging prefix

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
      "Rot",
      "Rst",
      "Fiber",
      "Suspend",
      "Resume",
      "Retire",
      "Underflow",
      "LdV",
      "LdC",
      "Ld",
      "St",
      "StV",
      "Tee",
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
      "Bind",
      "dBug"};

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 414122082387643854
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
