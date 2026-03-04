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
    Rst = 19,            // reset stack height to a fixed height
    Fiber = 20,            // Create new fiber
    Suspend = 21,            // suspend fiber
    Resume = 22,            // resume fiber
    Retire = 23,            // retire a fiber
    Underflow = 24,            // underflow from current stack
    LdV = 25,            // Place a void value on stack
    LdC = 26,            // load literal from constant pool
    Ld = 27,            // load stack from local[xx]
    St = 28,            // store tos to local[xx]
    StV = 29,            // clear a local to void
    Tee = 30,            // copy tos to local[xx]
    LdG = 31,            // load a global variable
    StG = 32,            // store into a global variable
    TG = 33,            // copy into a global variable
    Sav = 34,            // create a single assignment variable
    LdSav = 35,            // derefence a sav, break if not set
    TstSav = 36,            // test a sav, return a logical
    StSav = 37,            // store a value into a single assignment variable
    TSav = 38,            // update single assignment variable leave value on stack
    Cell = 39,            // create R/W cell
    Get = 40,            // access a R/W cell
    Assign = 41,            // assign to a R/W cell
    CLbl = 42,            // T,Lbl --> test for a data term, break if not lbl
    CInt = 43,            // T,lit --> test for a literal integer, break if not
    CChar = 44,            // T,lit --> test for a literal char, break if not
    CFlt = 45,            // T,lit --> test for a literal floating point, break if not
    CLit = 46,            // T,lit --> test for a literal value, break if not
    Nth = 47,            // T --> el, pick up the nth element
    StNth = 48,            // T el --> store in nth element
    If = 49,            // break if true
    IfNot = 50,            // break if false
    ICase = 51,            // T --> T, icase <Max>
    Case = 52,            // T --> T, case <Max>
    IxCase = 53,            // check and jump on type index
    IAdd = 54,            // L R --> L+R
    ISub = 55,            // L R --> L-R
    IMul = 56,            // L R --> L*R
    IDiv = 57,            // L R --> L/R
    IMod = 58,            // L R --> L%R
    IAbs = 59,            // L --> abs(L)
    IEq = 60,            // L R --> L==R
    ILt = 61,            // L R --> L<R
    IGe = 62,            // L R --> L>=R
    CEq = 63,            // L R --> L==R
    CLt = 64,            // L R --> L<R
    CGe = 65,            // L R --> L>=R
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
    Alloc = 82,            // new structure, elements from stack
    Closure = 83,            // allocate a closure
    Frame = 84,            // frame instruction
    Line = 85,            // source line indicator
    Bind = 86,            // bind a variable
    dBug = 87,            // debugging prefix

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
#define OPCODE_SIGNATURE 1472534314864623738
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
