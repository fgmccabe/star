#ifndef X86_64_H_
#define X86_64_H_

#include <config.h>

typedef struct assem_ctx *x64CtxPo;

typedef struct assem_lbl *x64LblPo;

x64CtxPo createCtx();
void *cleanupCtx(x64CtxPo ctx);

x64LblPo findLabel(x64CtxPo ctx, char *lName);
x64LblPo defineLabel(x64CtxPo ctx, char *lName, integer pc);
void setLabel(x64CtxPo ctx, x64LblPo lbl);
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
#define xchg(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; xchg_(d,s,ctx); } while(False)

void bswap_(x64Op dst, x64Op src, x64CtxPo ctx);
void xadd_(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg_(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg8b_(x64Op dst, x64Op src, x64CtxPo ctx);

void push_(x64Op src, x64CtxPo ctx);
#define push(src, ctx) do{ x64Op s=src; push_(s,ctx); } while(False)

void pop_(x64Op dst, x64CtxPo ctx);
#define pop(dst, ctx) do{ x64Op d=dst; pop_(d,ctx); } while(False)

void cwd_(x64Op dst, x64CtxPo ctx);
void cdq_(x64Op dst, x64CtxPo ctx);
void cbw_(x64Op src, x64CtxPo ctx);
void movsx_(x64Op dst, x64Op src, x64CtxPo ctx);
void movzx_(x64Op dst, x64Op src, x64CtxPo ctx);

void adcx_(x64Op dst, x64Op src, x64CtxPo ctx);
void adox_(x64Op dst, x64Op src, x64CtxPo ctx);

void add_(x64Op dst, x64Op src, x64CtxPo ctx);
#define add(dst,src,ctx) do{ x64Op d=dst; x64Op s = src; add_(d,s,ctx); } while(False)
void adc_(x64Op dst, x64Op src, x64CtxPo ctx);
#define adc(dst,src,ctx) do{ x64Op d=dst; x64Op s = src; adc_(d,s,ctx); } while(False)

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
#define and(dst,src,ctx) do{ x64Op d=dst; x64Op s = src; and_(d,s,ctx); } while(False)

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

#define EQ_CC 0x4
#define NE_CC 0x5
#define C_CC 0x2
#define AE_CC 0x3
#define NA_CC 0x6
#define A_CC 0x7
#define LT_CC 0xc
#define GE_CC 0xd
#define LE_CC 0xe
#define G_CC 0xf
#define OV_CC 0x0
#define NO_CC 0x1
#define PE_CC 0xa
#define PO_CC 0xb
#define S_CC 0x8
#define NS_CC 0x9

void setcc_(x64Reg dst, u8 cc, x64CtxPo ctx);
#define setne(dst,ctx) setcc_(dst,NE_CC,ctx)
#define seta(dst,ctx) setcc_(dst,A_CC,ctx)
#define setae(dst,ctx) setcc_(dst,AE_CC,ctx)
#define setg(dst,ctx) setcc_(dst,G_CC,ctx)
#define setge(dst,ctx) setcc_(dst,GE_CC,ctx)
#define setl(dst,ctx) setcc_(dst,LT_CC,ctx)
#define setle(dst,ctx) setcc_(dst,LE_CC,ctx)
#define sets(dst,ctx) setcc_(dst,S_CC,ctx)
#define setns(dst,ctx) setcc_(dst,NS_CC,ctx)
#define seto(dst,ctx) setcc_(dst,OV_CC,ctx)
#define setno(dst,ctx) setcc_(dst,NO_CC,ctx)
#define setpe(dst,ctx) setcc_(dst,PE_CC,ctx)
#define setpo(dst,ctx) setcc_(dst,PO_CC,ctx)

void test_(x64Op dst, x64Op src, x64CtxPo ctx);
#define test(dst,src,ctx) do{ x64Op d=dst; x64Op s = src; test_(d,s,ctx); } while(False)

void jmp_(x64Op dst, x64CtxPo ctx);
#define jmp(dst, ctx) do{ x64Op d=dst; jmp_(d,ctx); } while(False)

void j_cc_(x64LblPo dst, u8 cc, x64CtxPo ctx);
#define je(dst,cxt) j_cc_(dst,EQ_CC,ctx)
#define jne(dst,cxt) j_cc_(dst,NE_CC,ctx)
#define ja(dst,cxt) j_cc_(dst,A_CC,ctx)
#define jna(dst,cxt) j_cc_(dst,NA_CC,ctx)
#define jae(dst,cxt) j_cc_(dst,AE_CC,ctx)
#define jg(dst,cxt) j_cc_(dst,G_CC,ctx)
#define jge(dst,cxt) j_cc_(dst,GE_CC,ctx)
#define jnge(dst,cxt) j_cc_(dst,GE_CC,ctx)
#define jl(dst,cxt) j_cc_(dst,LT_CC,ctx)
#define jle(dst,cxt) j_cc_(dst,LE_CC,ctx)
#define jc(dst,cxt) j_cc_(dst,C_CC,ctx)
#define jnc(dst,cxt) j_cc_(dst,AE_CC,ctx)
#define jo(dst,cxt) j_cc_(dst,OV_CC,ctx)
#define jno(dst,cxt) j_cc_(dst,NO_CC,ctx)
#define jpe(dst,cxt) j_cc_(dst,PE_CC,ctx)
#define jpo(dst,cxt) j_cc_(dst,PO_CC,ctx)
#define js(dst,cxt) j_cc_(dst,S_CC,ctx)
#define jns(dst,cxt) j_cc_(dst,NS_CC,ctx)

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
