#ifndef X86_64_H_
#define X86_64_H_

#include "config.h"
#include "utils.h"

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

typedef struct x640p {
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
} FlexOp;

typedef x64Reg mcRegister;

logical sameFlexOp(FlexOp a, FlexOp b);

#define PLATFORM_PC_DELTA 4

#define RG(Rg) {.mode=Reg, .op.reg=(Rg)}
#define IM(Vl) {.mode=Immediate, .op.imm=(Vl)}
#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc, Off) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix), .op.indexed.disp=(Off), .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void lea_(x64Reg dst, FlexOp src, assemCtxPo ctx);
#define lea(dst, src, ctx) lea_(dst,(FlexOp)(src),ctx)

void mov_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define mov(dst, src, ctx) mov_((FlexOp)(dst),(FlexOp)(src),ctx)

void cmov_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void xchg_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define xchg(dst, src, ctx) xchg_((FlexOp)(dst),(FlexOp)(src),ctx)

void bswap_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void xadd_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void cmpxchg_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void cmpxchg8b_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void cmp_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void cqo_(assemCtxPo ctx); // sign extend rax into rdx:rax
void cdq_(assemCtxPo ctx); // sign extend eax into edx:eax

void push_(FlexOp src, assemCtxPo ctx);
#define push(src, ctx) =push_((FlexOp)(src),ctx)

void pop_(FlexOp dst, assemCtxPo ctx);
#define pop(dst, ctx) pop_((FlexOp)(dst),ctx)

void leave_(assemCtxPo ctx);

void cwd_(FlexOp dst, assemCtxPo ctx);
void cbw_(FlexOp src, assemCtxPo ctx);

void movzx_(x64Reg dst, FlexOp src, assemCtxPo ctx);
#define movzx(dst, src, ctx) movzx_(dst,(FlexOp)(src),ctx)

void movzdx_(x64Reg dst, FlexOp src, assemCtxPo ctx);
#define movzdx(dst, src, ctx) movzdx_(dst,(FlexOp)(src),ctx)

void fld(int i, assemCtxPo ctx);

void fadd_(assemCtxPo ctx);
void fsub_(assemCtxPo ctx);
void fmul_(assemCtxPo ctx);
void fdiv_(assemCtxPo ctx);

void add_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define add(dst, src, ctx) add_((FlexOp)(dst),(FlexOp)(src),ctx)
void adc_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define adc(dst, src, ctx) adc_((FlexOp)(dst),(FlexOp)(src),ctx)
void cmp_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define cmp(dst, src, ctx) cmp_((FlexOp)(dst),(FlexOp)(src),ctx)
void sub_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define sub(dst, src, ctx) sub_((FlexOp)(dst),(FlexOp)(src),ctx)
void sbb_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define sbb(dst, src, ctx) sbb_((FlexOp)(dst),(FlexOp)(src),ctx)

void imul_(x64Reg dst, FlexOp src, assemCtxPo ctx);
#define imul(dst, src, ctx) imul_(dst,(FlexOp)(src),ctx)

void idiv_(FlexOp src, assemCtxPo ctx);
#define idiv(src, ctx) idiv_((FlexOp)(src),ctx)

void inc_(FlexOp dst, assemCtxPo ctx);
#define inc(dst, ctx) inc_((FlexOp)(dst),ctx)

void dec_(FlexOp dst, assemCtxPo ctx);
#define dec(dst, ctx) dec_((FlexOp)(dst),ctx)

void and_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define and(dst, src, ctx) and_((FlexOp)(dst),(FlexOp)(src),ctx)

void or_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define or(dst, src, ctx) or_((FlexOp)(dst),(FlexOp)(src),ctx)

void xor_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define xor(dst, src, ctx) xor_((FlexOp)(dst),(FlexOp)(src),ctx)

void neg_(FlexOp dst, assemCtxPo ctx);
#define neg(dst, ctx) neg_((FlexOp)(dst),ctx)

void not_(FlexOp dst, assemCtxPo ctx);
#define not(dst, ctx) not_((FlexOp)(dst),ctx)

void sar_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void shr_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void sal_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void shl_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void shrd_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void shld_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void ror_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void rol_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void rcr_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void rcl_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void bt_(FlexOp src, assemCtxPo ctx);
void bts_(FlexOp src, assemCtxPo ctx);
void btr_(FlexOp src, assemCtxPo ctx);
void btc_(FlexOp src, assemCtxPo ctx);
void bsf_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void bsr_(FlexOp dst, FlexOp src, assemCtxPo ctx);

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

void test_(FlexOp dst, FlexOp src, assemCtxPo ctx);
#define test(dst, src, ctx) do{ x64Op d=dst; x64Op s = src; test_(d,s,ctx); } while(False)

void jmp_(FlexOp dst, assemCtxPo ctx);
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

void loop_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void loopz_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void loopnz_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void call_(FlexOp src, assemCtxPo ctx);
#define call(dst, ctx) do{ x64Op d=dst; call_(d,ctx); } while(False)

void ret_(int16 disp, assemCtxPo ctx);
#define ret(disp, ctx) ret_(disp,ctx)
#define rtn(ctx) ret_(0,ctx)

void stc_(assemCtxPo ctx);
void clc_(assemCtxPo ctx);
void cmc_(assemCtxPo ctx);

void std_(assemCtxPo ctx);
void cld_(assemCtxPo ctx);
void cpuid_(assemCtxPo ctx);
void hlt_();

void lahf_(assemCtxPo ctx);
void sahf_(assemCtxPo ctx);
void pushf_(assemCtxPo ctx);
void pushq_(assemCtxPo ctx);
void pushfd_(assemCtxPo ctx);
void pushfq_(assemCtxPo ctx);
void popf_(assemCtxPo ctx);
void popfd_(assemCtxPo ctx);
void popfq_(assemCtxPo ctx);
void popq_(assemCtxPo ctx);

void sti_(assemCtxPo ctx);
void cli_(assemCtxPo ctx);

void lds_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void les_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void lfs_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void lgs_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void lss_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void nop_(assemCtxPo ctx);

void xlat_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void xlatb_(FlexOp dst, FlexOp src, assemCtxPo ctx);

void movebe_(FlexOp dst, FlexOp src, assemCtxPo ctx);
void prefetchw_(FlexOp src, assemCtxPo ctx);
void prefetchwt1_(FlexOp src, assemCtxPo ctx);

void clflush_(FlexOp src, assemCtxPo ctx);
void clflushopt_(FlexOp src, assemCtxPo ctx);

void xsave_(FlexOp dst, assemCtxPo ctx);
void xsavec_(FlexOp dst, assemCtxPo ctx);
void xsaveopt_(FlexOp dst, assemCtxPo ctx);
void xrstor_(FlexOp dst, assemCtxPo ctx);
void xgetbv_(FlexOp dst, assemCtxPo ctx);

void rdrand_(FlexOp dst, assemCtxPo ctx);
void rdseed_(FlexOp dst, assemCtxPo ctx);

void mfence_(assemCtxPo ctx);
void lfence_(assemCtxPo ctx);
void pause_(assemCtxPo ctx);
#endif
