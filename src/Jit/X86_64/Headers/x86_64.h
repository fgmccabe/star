#ifndef X86_64_H_
#define X86_64_H_

#include <config.h>

typedef struct assem_ctx *assemCtxPo;
typedef struct assem_lbl *codeLblPo;

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
  Immediate,
  Based,
  Indexed,
  Labeled
} x64OpMode;

typedef struct {
  x64OpMode mode;
  union {
    x64Reg reg;
    struct {
      x64Reg base;
      int disp;
    } based;
    struct {
      x64Reg base;
      x64Reg index;
      int8 scale;
      int64 disp;
    } indexed;
    int64 imm;
    uint8 fpReg;
    codeLblPo lbl;
  } op;
} x64Op;

typedef x64Op registerSpec;

#define RG(Rg) {.mode=Reg, .op.reg=(Rg)}
#define IM(Vl) {.mode=Immediate, .op.imm=(Vl)}
#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc, Off) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix), .op.indexed.disp=(Off), .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void lea_(x64Reg dst, x64Op src, assemCtxPo ctx);
#define lea(dst, src, ctx) do{ x64Op s=src; lea_(dst,s,ctx); } while(False)

void mov_(x64Op dst, x64Op src, assemCtxPo ctx);
#define mov(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; mov_(d,s,ctx); } while(False)

void xchg_(x64Op dst, x64Op src, assemCtxPo ctx);
#define xchg(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; xchg_(d,s,ctx); } while(False)

void bswap_(x64Op dst, x64Op src, assemCtxPo ctx);
void xadd_(x64Op dst, x64Op src, assemCtxPo ctx);
void cmpxchg_(x64Op dst, x64Op src, assemCtxPo ctx);
void cmpxchg8b_(x64Op dst, x64Op src, assemCtxPo ctx);

void push_(x64Op src, assemCtxPo ctx);
#define push(src, ctx) do{ x64Op s=src; push_(s,ctx); } while(False)

void pop_(x64Op dst, assemCtxPo ctx);
#define pop(dst, ctx) do{ x64Op d=dst; pop_(d,ctx); } while(False)

void cwd_(x64Op dst, assemCtxPo ctx);
void cdq_(x64Op dst, assemCtxPo ctx);
void cbw_(x64Op src, assemCtxPo ctx);

void movsx_(x64Reg dst, x64Op src, uint8 scale, assemCtxPo ctx);
#define movsx(dst, src, scale, ctx) do{ x64Op s = src; movsx_(dst,s,scale,ctx); } while(False)

void movzx_(x64Reg dst, x64Op src, assemCtxPo ctx);
#define movzx(dst, src, ctx) do{ x64Op s = src; movzx_(dst,s,ctx); } while(False)

void movzdx_(x64Reg dst, x64Op src, assemCtxPo ctx);
#define movzdx(dst, src, ctx) do{ x64Op s = src; movzdx_(dst,s,ctx); } while(False)

void add_(x64Op dst, x64Op src, assemCtxPo ctx);
#define add(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; add_(d,s,ctx); } while(False)
void adc_(x64Op dst, x64Op src, assemCtxPo ctx);
#define adc(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; adc_(d,s,ctx); } while(False)
void cmp_(x64Op dst, x64Op src, assemCtxPo ctx);
#define cmp(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; cmp_(d,s,ctx); } while(False)
void sub_(x64Op dst, x64Op src, assemCtxPo ctx);
#define sub(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; sub_(d,s,ctx); } while(False)
void sbb_(x64Op dst, x64Op src, assemCtxPo ctx);
#define sbb(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; sbb_(d,s,ctx); } while(False)

void imul_(x64Reg dst, x64Op src, assemCtxPo ctx);
#define imul(dst, src, ctx) do{ x64Op s = src; imul_(dst,s,ctx); } while(False)

void idiv_(x64Op src, assemCtxPo ctx);
#define idiv(src, ctx) do{ x64Op s = src; idiv_(s,ctx); } while(False)

void inc_(x64Op dst, assemCtxPo ctx);
#define inc(dst, ctx) do{ x64Op d=dst; inc_(d,ctx); } while(False)

void dec_(x64Op dst, assemCtxPo ctx);
#define dec(dst, ctx) do{ x64Op d=dst; dec_(d,ctx); } while(False)

void and_(x64Op dst, x64Op src, assemCtxPo ctx);
#define and(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; and_(d,s,ctx); } while(False)

void or_(x64Op dst, x64Op src, assemCtxPo ctx);
#define or(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; or_(d,s,ctx); } while(False)

void xor_(x64Op dst, x64Op src, assemCtxPo ctx);
#define xor(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; xor_(d,s,ctx); } while(False)

void neg_(x64Op dst, assemCtxPo ctx);
#define neg(dst, ctx) do{ x64Op d=dst; neg_(d,ctx); } while(False)

void not_(x64Op dst, assemCtxPo ctx);
#define not(dst, ctx) do{ x64Op d=dst; not_(d,ctx); } while(False)

void sar_(x64Op dst, x64Op src, assemCtxPo ctx);
void shr_(x64Op dst, x64Op src, assemCtxPo ctx);
void sal_(x64Op dst, x64Op src, assemCtxPo ctx);
void shl_(x64Op dst, x64Op src, assemCtxPo ctx);
void shrd_(x64Op dst, x64Op src, assemCtxPo ctx);
void shld_(x64Op dst, x64Op src, assemCtxPo ctx);

void ror_(x64Op dst, x64Op src, assemCtxPo ctx);
void rol_(x64Op dst, x64Op src, assemCtxPo ctx);
void rcr_(x64Op dst, x64Op src, assemCtxPo ctx);
void rcl_(x64Op dst, x64Op src, assemCtxPo ctx);

void bt_(x64Op src, assemCtxPo ctx);
void bts_(x64Op src, assemCtxPo ctx);
void btr_(x64Op src, assemCtxPo ctx);
void btc_(x64Op src, assemCtxPo ctx);
void bsf_(x64Op dst, x64Op src, assemCtxPo ctx);
void bsr_(x64Op dst, x64Op src, assemCtxPo ctx);

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

void setcc_(x64Reg dst, uint8 cc, assemCtxPo ctx);
#define setne(dst, ctx) setcc_(dst,NE_CC,ctx)
#define seta(dst, ctx) setcc_(dst,A_CC,ctx)
#define setae(dst, ctx) setcc_(dst,AE_CC,ctx)
#define setg(dst, ctx) setcc_(dst,G_CC,ctx)
#define setge(dst, ctx) setcc_(dst,GE_CC,ctx)
#define setl(dst, ctx) setcc_(dst,LT_CC,ctx)
#define setle(dst, ctx) setcc_(dst,LE_CC,ctx)
#define sets(dst, ctx) setcc_(dst,S_CC,ctx)
#define setns(dst, ctx) setcc_(dst,NS_CC,ctx)
#define seto(dst, ctx) setcc_(dst,OV_CC,ctx)
#define setno(dst, ctx) setcc_(dst,NO_CC,ctx)
#define setpe(dst, ctx) setcc_(dst,PE_CC,ctx)
#define setpo(dst, ctx) setcc_(dst,PO_CC,ctx)

void test_(x64Op dst, x64Op src, assemCtxPo ctx);
#define test(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; test_(d,s,ctx); } while(False)

void jmp_(x64Op dst, assemCtxPo ctx);
#define jmp(dst, ctx) do{ x64Op d=dst; jmp_(d,ctx); } while(False)

void j_cc_(codeLblPo dst, uint8 cc, assemCtxPo ctx);
#define je(dst, cxt) j_cc_(dst,EQ_CC,ctx)
#define jne(dst, cxt) j_cc_(dst,NE_CC,ctx)
#define ja(dst, cxt) j_cc_(dst,A_CC,ctx)
#define jna(dst, cxt) j_cc_(dst,NA_CC,ctx)
#define jae(dst, cxt) j_cc_(dst,AE_CC,ctx)
#define jg(dst, cxt) j_cc_(dst,G_CC,ctx)
#define jge(dst, cxt) j_cc_(dst,GE_CC,ctx)
#define jnge(dst, cxt) j_cc_(dst,GE_CC,ctx)
#define jl(dst, cxt) j_cc_(dst,LT_CC,ctx)
#define jle(dst, cxt) j_cc_(dst,LE_CC,ctx)
#define jc(dst, cxt) j_cc_(dst,C_CC,ctx)
#define jnc(dst, cxt) j_cc_(dst,AE_CC,ctx)
#define jo(dst, cxt) j_cc_(dst,OV_CC,ctx)
#define jno(dst, cxt) j_cc_(dst,NO_CC,ctx)
#define jpe(dst, cxt) j_cc_(dst,PE_CC,ctx)
#define jpo(dst, cxt) j_cc_(dst,PO_CC,ctx)
#define js(dst, cxt) j_cc_(dst,S_CC,ctx)
#define jns(dst, cxt) j_cc_(dst,NS_CC,ctx)

void loop_(x64Op dst, x64Op src, assemCtxPo ctx);
void loopz_(x64Op dst, x64Op src, assemCtxPo ctx);
void loopnz_(x64Op dst, x64Op src, assemCtxPo ctx);

void call_(x64Op src, assemCtxPo ctx);
#define call(dst, ctx) do{ x64Op d=dst; call_(d,ctx); } while(False)

void ret_(int16 disp, assemCtxPo ctx);
#define ret(disp, ctx) ret_(disp,ctx)
#define rtn(ctx) ret_(0,ctx)

void stc_(assemCtxPo ctx);
void clc_(assemCtxPo ctx);
void cmc_(assemCtxPo ctx);

void std_(assemCtxPo ctx);
void cld_(assemCtxPo ctx);

void lahf_(assemCtxPo ctx);
void sahf_(assemCtxPo ctx);
void pushf_(assemCtxPo ctx);
void pushfd_(assemCtxPo ctx);
void popf_(assemCtxPo ctx);
void popfd_(assemCtxPo ctx);

void sti_(assemCtxPo ctx);
void cli_(assemCtxPo ctx);

void lds_(x64Op dst, x64Op src, assemCtxPo ctx);
void les_(x64Op dst, x64Op src, assemCtxPo ctx);
void lfs_(x64Op dst, x64Op src, assemCtxPo ctx);
void lgs_(x64Op dst, x64Op src, assemCtxPo ctx);
void lss_(x64Op dst, x64Op src, assemCtxPo ctx);

void nop_(assemCtxPo ctx);

void xlat_(x64Op dst, x64Op src, assemCtxPo ctx);
void xlatb_(x64Op dst, x64Op src, assemCtxPo ctx);

void cpuid_(x64Op dst, assemCtxPo ctx);
void movebe_(x64Op dst, x64Op src, assemCtxPo ctx);
void prefetchw_(x64Op src, assemCtxPo ctx);
void prefetchwt1_(x64Op src, assemCtxPo ctx);

void clflush_(x64Op src, assemCtxPo ctx);
void clflushopt_(x64Op src, assemCtxPo ctx);

void xsave_(x64Op dst, assemCtxPo ctx);
void xsavec_(x64Op dst, assemCtxPo ctx);
void xsaveopt_(x64Op dst, assemCtxPo ctx);
void xrstor_(x64Op dst, assemCtxPo ctx);
void xgetbv_(x64Op dst, assemCtxPo ctx);

void rdrand_(x64Op dst, assemCtxPo ctx);
void rdseed_(x64Op dst, assemCtxPo ctx);

#endif
