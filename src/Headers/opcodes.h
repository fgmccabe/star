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
    LdA = 28,            // load stack from args[xx]
    LdL = 29,            // load stack from local[xx]
    StL = 30,            // store tos to local[xx]
    StV = 31,            // clear a local to void
    TL = 32,            // copy tos to local[xx]
    LdG = 33,            // load a global variable
    StG = 34,            // store into a global variable
    TG = 35,            // copy into a global variable
    Sav = 36,            // create a single assignment variable
    LdSav = 37,            // derefence a sav, break if not set
    TstSav = 38,            // test a sav, return a logical
    StSav = 39,            // store a value into a single assignment variable
    TSav = 40,            // update single assignment variable leave value on stack
    Cell = 41,            // create R/W cell
    Get = 42,            // access a R/W cell
    Assign = 43,            // assign to a R/W cell
    CLbl = 44,            // T,Lbl --> test for a data term, break if not lbl
    CInt = 45,            // T,lit --> test for a literal integer, break if not
    CChar = 46,            // T,lit --> test for a literal char, break if not
    CFlt = 47,            // T,lit --> test for a literal floating point, break if not
    CLit = 48,            // T,lit --> test for a literal value, break if not
    Nth = 49,            // T --> el, pick up the nth element
    StNth = 50,            // T el --> store in nth element
    If = 51,            // break if true
    IfNot = 52,            // break if false
    ICase = 53,            // T --> T, icase <Max>
    Case = 54,            // T --> T, case <Max>
    IxCase = 55,            // check and jump on type index
    IAdd = 56,            // L R --> L+R
    ISub = 57,            // L R --> L-R
    IMul = 58,            // L R --> L*R
    IDiv = 59,            // L R --> L/R
    IMod = 60,            // L R --> L%R
    IAbs = 61,            // L --> abs(L)
    IEq = 62,            // L R --> L==R
    ILt = 63,            // L R --> L<R
    IGe = 64,            // L R --> L>=R
    CEq = 65,            // L R --> L==R
    CLt = 66,            // L R --> L<R
    CGe = 67,            // L R --> L>=R
    BAnd = 68,            // L R --> L&R
    BOr = 69,            // L R --> L|R
    BXor = 70,            // L R --> L^R
    BLsl = 71,            // L R --> L<<R
    BLsr = 72,            // L R --> L>>R
    BAsr = 73,            // L R --> L>>>R
    BNot = 74,            // L --> ~L
    FAdd = 75,            // L R --> L+R
    FSub = 76,            // L R --> L-R
    FMul = 77,            // L R --> L*R
    FDiv = 78,            // L R --> L/R
    FMod = 79,            // L R --> L%R
    FAbs = 80,            // L --> abs(L)
    FEq = 81,            // L R e --> L==R
    FLt = 82,            // L R --> L<R
    FGe = 83,            // L R --> L>=R
    Alloc = 84,            // new structure, elements from stack
    Closure = 85,            // allocate a closure
    Frame = 86,            // frame instruction
    Line = 87,            // source line indicator
    Bind = 88,            // bind a variable
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
      "Bind",
      "dBug"};

#ifndef OPCODE_SIGNATURE
#define OPCODE_SIGNATURE 1207279009152735333
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
