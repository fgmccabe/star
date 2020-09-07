#ifndef X86_64_H_
#define X86_64_H_

#include <config.h>
#include "jitP.h"

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
    codeLblPo lbl;
  } op;
} x64Op;

#define RG(Rg) {.mode=Reg, .op.reg=(Rg)}
#define IM(Vl) {.mode=Immediate, .op.imm=(Vl)}
#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc, Off) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix), .op.indexed.disp=(Off), .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void lea_(x64Reg dst, x64Op src, codeCtxPo ctx);
#define lea(dst, src, ctx) do{ x64Op s=src; lea_(dst,s,ctx); } while(False)

void mov_(x64Op dst, x64Op src, codeCtxPo ctx);
#define mov(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; mov_(d,s,ctx); } while(False)

void xchg_(x64Op dst, x64Op src, codeCtxPo ctx);
#define xchg(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; xchg_(d,s,ctx); } while(False)

void bswap_(x64Op dst, x64Op src, codeCtxPo ctx);
void xadd_(x64Op dst, x64Op src, codeCtxPo ctx);
void cmpxchg_(x64Op dst, x64Op src, codeCtxPo ctx);
void cmpxchg8b_(x64Op dst, x64Op src, codeCtxPo ctx);

void push_(x64Op src, codeCtxPo ctx);
#define push(src, ctx) do{ x64Op s=src; push_(s,ctx); } while(False)

void pop_(x64Op dst, codeCtxPo ctx);
#define pop(dst, ctx) do{ x64Op d=dst; pop_(d,ctx); } while(False)

void cwd_(x64Op dst, codeCtxPo ctx);
void cdq_(x64Op dst, codeCtxPo ctx);
void cbw_(x64Op src, codeCtxPo ctx);

void movsx_(x64Reg dst, x64Op src, u8 scale, codeCtxPo ctx);
#define movsx(dst, src, scale, ctx) do{ x64Op s = src; movsx_(dst,s,scale,ctx); } while(False)

void movzx_(x64Reg dst, x64Op src, codeCtxPo ctx);
#define movzx(dst, src, ctx) do{ x64Op s = src; movzx_(dst,s,ctx); } while(False)

void movzdx_(x64Reg dst, x64Op src, codeCtxPo ctx);
#define movzdx(dst, src, ctx) do{ x64Op s = src; movzdx_(dst,s,ctx); } while(False)

void add_(x64Op dst, x64Op src, codeCtxPo ctx);
#define add(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; add_(d,s,ctx); } while(False)
void adc_(x64Op dst, x64Op src, codeCtxPo ctx);
#define adc(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; adc_(d,s,ctx); } while(False)
void cmp_(x64Op dst, x64Op src, codeCtxPo ctx);
#define cmp(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; cmp_(d,s,ctx); } while(False)
void sub_(x64Op dst, x64Op src, codeCtxPo ctx);
#define sub(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; sub_(d,s,ctx); } while(False)
void sbb_(x64Op dst, x64Op src, codeCtxPo ctx);
#define sbb(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; sbb_(d,s,ctx); } while(False)

void imul_(x64Reg dst, x64Op src, codeCtxPo ctx);
#define imul(dst, src, ctx) do{ x64Op s = src; imul_(dst,s,ctx); } while(False)

void idiv_(x64Op src, codeCtxPo ctx);
#define idiv(src, ctx) do{ x64Op s = src; idiv_(s,ctx); } while(False)

void inc_(x64Op dst, codeCtxPo ctx);
#define inc(dst, ctx) do{ x64Op d=dst; inc_(d,ctx); } while(False)

void dec_(x64Op dst, codeCtxPo ctx);
#define dec(dst, ctx) do{ x64Op d=dst; dec_(d,ctx); } while(False)

void and_(x64Op dst, x64Op src, codeCtxPo ctx);
#define and(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; and_(d,s,ctx); } while(False)

void or_(x64Op dst, x64Op src, codeCtxPo ctx);
#define or(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; or_(d,s,ctx); } while(False)

void xor_(x64Op dst, x64Op src, codeCtxPo ctx);
#define xor(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; xor_(d,s,ctx); } while(False)

void neg_(x64Op dst, codeCtxPo ctx);
#define neg(dst, ctx) do{ x64Op d=dst; neg_(d,ctx); } while(False)

void not_(x64Op dst, codeCtxPo ctx);
#define not(dst, ctx) do{ x64Op d=dst; not_(d,ctx); } while(False)

void sar_(x64Op dst, x64Op src, codeCtxPo ctx);
void shr_(x64Op dst, x64Op src, codeCtxPo ctx);
void sal_(x64Op dst, x64Op src, codeCtxPo ctx);
void shl_(x64Op dst, x64Op src, codeCtxPo ctx);
void shrd_(x64Op dst, x64Op src, codeCtxPo ctx);
void shld_(x64Op dst, x64Op src, codeCtxPo ctx);

void ror_(x64Op dst, x64Op src, codeCtxPo ctx);
void rol_(x64Op dst, x64Op src, codeCtxPo ctx);
void rcr_(x64Op dst, x64Op src, codeCtxPo ctx);
void rcl_(x64Op dst, x64Op src, codeCtxPo ctx);

void bt_(x64Op src, codeCtxPo ctx);
void bts_(x64Op src, codeCtxPo ctx);
void btr_(x64Op src, codeCtxPo ctx);
void btc_(x64Op src, codeCtxPo ctx);
void bsf_(x64Op dst, x64Op src, codeCtxPo ctx);
void bsr_(x64Op dst, x64Op src, codeCtxPo ctx);

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

void setcc_(x64Reg dst, u8 cc, codeCtxPo ctx);
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

void test_(x64Op dst, x64Op src, codeCtxPo ctx);
#define test(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; test_(d,s,ctx); } while(False)

void jmp_(x64Op dst, codeCtxPo ctx);
#define jmp(dst, ctx) do{ x64Op d=dst; jmp_(d,ctx); } while(False)

void j_cc_(codeLblPo dst, u8 cc, codeCtxPo ctx);
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

void loop_(x64Op dst, x64Op src, codeCtxPo ctx);
void loopz_(x64Op dst, x64Op src, codeCtxPo ctx);
void loopnz_(x64Op dst, x64Op src, codeCtxPo ctx);

void call_(x64Op src, codeCtxPo ctx);
#define call(dst, ctx) do{ x64Op d=dst; call_(d,ctx); } while(False)

void ret_(i16 disp, codeCtxPo ctx);
#define ret(disp, ctx) ret_(disp,ctx)
#define rtn(ctx) ret_(0,ctx)

void stc_(codeCtxPo ctx);
void clc_(codeCtxPo ctx);
void cmc_(codeCtxPo ctx);

void std_(codeCtxPo ctx);
void cld_(codeCtxPo ctx);

void lahf_(codeCtxPo ctx);
void sahf_(codeCtxPo ctx);
void pushf_(codeCtxPo ctx);
void pushfd_(codeCtxPo ctx);
void popf_(codeCtxPo ctx);
void popfd_(codeCtxPo ctx);

void sti_(codeCtxPo ctx);
void cli_(codeCtxPo ctx);

void lds_(x64Op dst, x64Op src, codeCtxPo ctx);
void les_(x64Op dst, x64Op src, codeCtxPo ctx);
void lfs_(x64Op dst, x64Op src, codeCtxPo ctx);
void lgs_(x64Op dst, x64Op src, codeCtxPo ctx);
void lss_(x64Op dst, x64Op src, codeCtxPo ctx);

void nop_(codeCtxPo ctx);

void xlat_(x64Op dst, x64Op src, codeCtxPo ctx);
void xlatb_(x64Op dst, x64Op src, codeCtxPo ctx);

void cpuid_(x64Op dst, codeCtxPo ctx);
void movebe_(x64Op dst, x64Op src, codeCtxPo ctx);
void prefetchw_(x64Op src, codeCtxPo ctx);
void prefetchwt1_(x64Op src, codeCtxPo ctx);

void clflush_(x64Op src, codeCtxPo ctx);
void clflushopt_(x64Op src, codeCtxPo ctx);

void xsave_(x64Op dst, codeCtxPo ctx);
void xsavec_(x64Op dst, codeCtxPo ctx);
void xsaveopt_(x64Op dst, codeCtxPo ctx);
void xrstor_(x64Op dst, codeCtxPo ctx);
void xgetbv_(x64Op dst, codeCtxPo ctx);

void rdrand_(x64Op dst, codeCtxPo ctx);
void rdseed_(x64Op dst, codeCtxPo ctx);

#endif
