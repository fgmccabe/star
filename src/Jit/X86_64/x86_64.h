#ifndef X86_64_H_
#define X86_64_H_

#include <config.h>

typedef struct assem_ctx *x64CtxPo;

typedef struct assem_lbl *x64LblPo;

x64CtxPo createCtx();
void eraseCtx(x64CtxPo ctx);

x64LblPo findLabel(x64CtxPo ctx, char *lName);
x64LblPo declareLabel(x64CtxPo ctx, char *lName, integer pc);
logical isLabelDefined(x64LblPo lbl);

typedef void (*lblRefUpdater)(x64CtxPo ctx, x64LblPo lbl, integer pc);
retCode addLabelReference(x64CtxPo ctx, x64LblPo lbl, integer pc, lblRefUpdater updater);
static retCode updateLblEntry(void *entry, integer ix, void *cl);

typedef enum {
  RAX = 0,
  RCX = 1,
  RDX = 2,
  RBX = 3,
  RSP = 4,
  RBP = 5,
  RSI = 6,
  RDI = 7,
  R8 = 8,
  R9 = 9,
  R10 = 10,
  R11 = 11,
  R12 = 12,
  R13 = 13,
  R14 = 14,
  R15 = 15
} x64Reg;

typedef enum {
  EQ = 0x84,
  NE = 0x85,
  C = 0x82,
  NC = 0x83,
  BE = 0x86,
  NBE = 0x87,
  LT = 0x8c,
  NL = 0x8d,
  LE = 0x8e,
  NLE = 0x8f,
  OV = 0x80,
  NO = 0x81,
  PE = 0x8a,
  PO = 0x8b
} x64Cond;

typedef enum {
  Reg,
  Imm,
  Based,
  Indexed,
  Labeled
} x64OpMode;

typedef unsigned char u8;
typedef signed char i8;

typedef int16 i16;
typedef uint16 u16;

typedef uint32 u32;
typedef int32 i32;

typedef int64 i64;
typedef uint64 u64;

typedef struct {
  x64OpMode mode;
  union {
    x64Reg reg;
    struct {
      x64Reg base;
      i64 disp;
    } based;
    struct {
      x64Reg base;
      x64Reg index;
      i8 scale;
      i64 disp;
    } indexed;
    i64 imm;
    u8 fpReg;
    x64LblPo lbl;
  } op;
} x64Op;

#define RG(Rg) {.mode=Reg, .op.reg=(Rg)}
#define IM(Vl) {.mode=Imm, .op.imm=(Vl)}
#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc, Off) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix), .op.indexed.disp=(Off), .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void lea_(x64Reg dst, x64Op src, x64CtxPo ctx);
#define lea(dst, src, ctx) do{ x64Op s=src; lea_(dst,s,ctx); } while(False)

void mov_(x64Op dst, x64Op src, x64CtxPo ctx);
#define mov(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; mov_(d,s,ctx); } while(False)

void xchg_(x64Op dst, x64Op src, x64CtxPo ctx);
void bswap_(x64Op dst, x64Op src, x64CtxPo ctx);
void xadd_(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg_(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg8b_(x64Op dst, x64Op src, x64CtxPo ctx);

void push_(x64Op src, x64CtxPo ctx);
#define push(src, ctx) do{ x64Op s=src; push_(s,ctx); } while(False)

void pop_(x64Op dst, x64CtxPo ctx);
#define pop(dst, ctx) do{ x64Op d=dst; pop_(d,ctx); } while(False)

void pushad_(x64Op src, x64CtxPo ctx);
void popad_(x64Op src, x64CtxPo ctx);
void pusha_(x64Op src, x64CtxPo ctx);
void popa_(x64Op src, x64CtxPo ctx);
void cwd_(x64Op dst, x64CtxPo ctx);
void cdq_(x64Op dst, x64CtxPo ctx);
void cbw_(x64Op src, x64CtxPo ctx);
void movsx_(x64Op dst, x64Op src, x64CtxPo ctx);
void movzx_(x64Op dst, x64Op src, x64CtxPo ctx);

void adcx_(x64Op dst, x64Op src, x64CtxPo ctx);
void adox_(x64Op dst, x64Op src, x64CtxPo ctx);
void add_(x64Op dst, x64Op src, x64CtxPo ctx);
void addc_(x64Op dst, x64Op src, x64CtxPo ctx);
void sub_(x64Op dst, x64Op src, x64CtxPo ctx);
void sbb_(x64Op dst, x64Op src, x64CtxPo ctx);
void imul_(x64Op dst, x64Op src, x64CtxPo ctx);
void mul_(x64Op dst, x64Op src, x64CtxPo ctx);
void idiv_(x64Op dst, x64Op src, x64CtxPo ctx);
void div_(x64Op dst, x64Op src, x64CtxPo ctx);
void inc_(x64Op dst, x64CtxPo ctx);
void dec_(x64Op dst, x64CtxPo ctx);
void neg_(x64Op dst, x64CtxPo ctx);
void cmp_(x64Op dst, x64CtxPo ctx);
void daa_(x64Op dst, x64Op src, x64CtxPo ctx);
void das_(x64Op dst, x64Op src, x64CtxPo ctx);
void aaa_(x64Op dst, x64Op src, x64CtxPo ctx);
void aas_(x64Op dst, x64Op src, x64CtxPo ctx);
void aam_(x64Op dst, x64Op src, x64CtxPo ctx);
void aad_(x64Op dst, x64Op src, x64CtxPo ctx);

void and_(x64Op dst, x64Op src, x64CtxPo ctx);
void or_(x64Op dst, x64Op src, x64CtxPo ctx);
void xor_(x64Op dst, x64Op src, x64CtxPo ctx);
void not_(x64Op dst, x64CtxPo ctx);

void sar_(x64Op dst, x64Op src, x64CtxPo ctx);
void shr_(x64Op dst, x64Op src, x64CtxPo ctx);
void sal_(x64Op dst, x64Op src, x64CtxPo ctx);
void shl_(x64Op dst, x64Op src, x64CtxPo ctx);
void shrd_(x64Op dst, x64Op src, x64CtxPo ctx);
void shld_(x64Op dst, x64Op src, x64CtxPo ctx);

void ror_(x64Op dst, x64Op src, x64CtxPo ctx);
void rol_(x64Op dst, x64Op src, x64CtxPo ctx);
void rcr_(x64Op dst, x64Op src, x64CtxPo ctx);
void rcl_(x64Op dst, x64Op src, x64CtxPo ctx);

void bt_(x64Op src, x64CtxPo ctx);
void bts_(x64Op src, x64CtxPo ctx);
void btr_(x64Op src, x64CtxPo ctx);
void btc_(x64Op src, x64CtxPo ctx);
void bsf_(x64Op dst, x64Op src, x64CtxPo ctx);
void bsr_(x64Op dst, x64Op src, x64CtxPo ctx);

void sete_(x64Op dst, x64Op src, x64CtxPo ctx);
void setne_(x64Op dst, x64Op src, x64CtxPo ctx);
void seta_(x64Op dst, x64Op src, x64CtxPo ctx);
void setae_(x64Op dst, x64Op src, x64CtxPo ctx);
void setg_(x64Op dst, x64Op src, x64CtxPo ctx);
void setge_(x64Op dst, x64Op src, x64CtxPo ctx);
void setl_(x64Op dst, x64Op src, x64CtxPo ctx);
void setle_(x64Op dst, x64Op src, x64CtxPo ctx);
void sets_(x64Op dst, x64Op src, x64CtxPo ctx);
void setns_(x64Op dst, x64Op src, x64CtxPo ctx);
void seto_(x64Op dst, x64Op src, x64CtxPo ctx);
void setno_(x64Op dst, x64Op src, x64CtxPo ctx);
void setpe_(x64Op dst, x64Op src, x64CtxPo ctx);
void setpo_(x64Op dst, x64Op src, x64CtxPo ctx);

void test_(x64Op dst, x64Op src, x64CtxPo ctx);

void jmp_(x64Op dst, x64CtxPo ctx);
#define jmp(dst, ctx) do{ x64Op d=dst; jmp_(d,ctx); } while(False)

void je_(x64Op dst, x64CtxPo ctx);
void jne_(x64Op dst, x64CtxPo ctx);
void ja_(x64Op dst, x64CtxPo ctx);
void jae_(x64Op dst, x64CtxPo ctx);
void jg_(x64Op dst, x64CtxPo ctx);
void jge_(x64Op dst, x64CtxPo ctx);
void jl_(x64Op dst, x64CtxPo ctx);
void jle_(x64Op dst, x64CtxPo ctx);
void jc_(x64Op dst, x64CtxPo ctx);
void jnc_(x64Op dst, x64CtxPo ctx);
void jo_(x64Op dst, x64CtxPo ctx);
void jno_(x64Op dst, x64CtxPo ctx);
void jpe_(x64Op dst, x64CtxPo ctx);
void jpo_(x64Op dst, x64CtxPo ctx);
void jcxz_(x64Op dst, x64CtxPo ctx);

void loop_(x64Op dst, x64Op src, x64CtxPo ctx);
void loopz_(x64Op dst, x64Op src, x64CtxPo ctx);
void loopnz_(x64Op dst, x64Op src, x64CtxPo ctx);

void call_(x64Op src, x64CtxPo ctx);
void ret_(x64CtxPo ctx);
void iret_(x64CtxPo ctx);
void intrupt_(x64CtxPo ctx);
void into_(x64CtxPo ctx);

void bound_(x64Op dst, x64Op src, x64CtxPo ctx);

void enter_(x64Op dst, x64Op src, x64CtxPo ctx);
void leave_(x64Op dst, x64Op src, x64CtxPo ctx);

void stc_(x64CtxPo ctx);
void clc_(x64CtxPo ctx);
void cmc_(x64CtxPo ctx);

void std_(x64CtxPo ctx);
void cld_(x64CtxPo ctx);

void lahf_(x64CtxPo ctx);
void sahf_(x64CtxPo ctx);
void pushf_(x64CtxPo ctx);
void pushfd_(x64CtxPo ctx);
void popf_(x64CtxPo ctx);
void popfd_(x64CtxPo ctx);

void sti_(x64CtxPo ctx);
void cli_(x64CtxPo ctx);

void lds_(x64Op dst, x64Op src, x64CtxPo ctx);
void les_(x64Op dst, x64Op src, x64CtxPo ctx);
void lfs_(x64Op dst, x64Op src, x64CtxPo ctx);
void lgs_(x64Op dst, x64Op src, x64CtxPo ctx);
void lss_(x64Op dst, x64Op src, x64CtxPo ctx);

void nop_(x64CtxPo ctx);

void xlat_(x64Op dst, x64Op src, x64CtxPo ctx);
void xlatb_(x64Op dst, x64Op src, x64CtxPo ctx);

void cpuid_(x64Op dst, x64CtxPo ctx);
void movebe_(x64Op dst, x64Op src, x64CtxPo ctx);
void prefetchw_(x64Op src, x64CtxPo ctx);
void prefetchwt1_(x64Op src, x64CtxPo ctx);

void clflush_(x64Op src, x64CtxPo ctx);
void clflushopt_(x64Op src, x64CtxPo ctx);

void xsave_(x64Op dst, x64CtxPo ctx);
void xsavec_(x64Op dst, x64CtxPo ctx);
void xsaveopt_(x64Op dst, x64CtxPo ctx);
void xrstor_(x64Op dst, x64CtxPo ctx);
void xgetbv_(x64Op dst, x64CtxPo ctx);

void rdrand_(x64Op dst, x64CtxPo ctx);
void rdseed_(x64Op dst, x64CtxPo ctx);

#endif
