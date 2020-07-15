#ifndef X86_64_H_
#define X86_64_H_

typedef struct assem_ctx *x64CtxPo;

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
} x64OpMode;

typedef unsigned char u8;
typedef signed char i8;

typedef short i16;
typedef unsigned short u16;

typedef unsigned long u32;
typedef long i32;

typedef long long i64;
typedef unsigned long long u64;

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
  } op;
} x64Op;

void mov(x64Op dst, x64Op src, x64CtxPo ctx);
void cmov(x64Op dst, x64Op src, x64Cond, x64CtxPo ctx);
void xchg(x64Op dst, x64Op src, x64CtxPo ctx);
void bswap(x64Op dst, x64Op src, x64CtxPo ctx);
void xadd(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg(x64Op dst, x64Op src, x64CtxPo ctx);
void cmpxchg8b(x64Op dst, x64Op src, x64CtxPo ctx);
void push(x64Op src, x64CtxPo ctx);
void pop(x64Op src, x64CtxPo ctx);
void pushad(x64Op src, x64CtxPo ctx);
void popad(x64Op src, x64CtxPo ctx);
void pusha(x64Op src, x64CtxPo ctx);
void popa(x64Op src, x64CtxPo ctx);
void cwd(x64Op dst, x64CtxPo ctx);
void cdq(x64Op dst, x64CtxPo ctx);
void cbw(x64Op src, x64CtxPo ctx);
void movsx(x64Op dst, x64Op src, x64CtxPo ctx);
void movzx(x64Op dst, x64Op src, x64CtxPo ctx);

void adcx(x64Op dst, x64Op src, x64CtxPo ctx);
void adox(x64Op dst, x64Op src, x64CtxPo ctx);
void add(x64Op dst, x64Op src, x64CtxPo ctx);
void addc(x64Op dst, x64Op src, x64CtxPo ctx);
void sub(x64Op dst, x64Op src, x64CtxPo ctx);
void sbb(x64Op dst, x64Op src, x64CtxPo ctx);
void imul(x64Op dst, x64Op src, x64CtxPo ctx);
void mul(x64Op dst, x64Op src, x64CtxPo ctx);
void idiv(x64Op dst, x64Op src, x64CtxPo ctx);
void div(x64Op dst, x64Op src, x64CtxPo ctx);
void inc(x64Op dst, x64CtxPo ctx);
void dec(x64Op dst, x64CtxPo ctx);
void neg(x64Op dst, x64CtxPo ctx);
void cmp(x64Op dst, x64CtxPo ctx);
void daa(x64Op dst, x64Op src, x64CtxPo ctx);
void das(x64Op dst, x64Op src, x64CtxPo ctx);
void aaa(x64Op dst, x64Op src, x64CtxPo ctx);
void aas(x64Op dst, x64Op src, x64CtxPo ctx);
void aam(x64Op dst, x64Op src, x64CtxPo ctx);
void aad(x64Op dst, x64Op src, x64CtxPo ctx);

void and(x64Op dst, x64Op src, x64CtxPo ctx);
void or(x64Op dst, x64Op src, x64CtxPo ctx);
void xor(x64Op dst, x64Op src, x64CtxPo ctx);
void not(x64Op dst, x64CtxPo ctx);

void sar(x64Op dst, x64Op src, x64CtxPo ctx);
void shr(x64Op dst, x64Op src, x64CtxPo ctx);
void sal(x64Op dst, x64Op src, x64CtxPo ctx);
void shl(x64Op dst, x64Op src, x64CtxPo ctx);
void shrd(x64Op dst, x64Op src, x64CtxPo ctx);
void shld(x64Op dst, x64Op src, x64CtxPo ctx);

void ror(x64Op dst, x64Op src, x64CtxPo ctx);
void rol(x64Op dst, x64Op src, x64CtxPo ctx);
void rcr(x64Op dst, x64Op src, x64CtxPo ctx);
void rcl(x64Op dst, x64Op src, x64CtxPo ctx);

void bt(x64Op src, x64CtxPo ctx);
void bts(x64Op src, x64CtxPo ctx);
void btr(x64Op src, x64CtxPo ctx);
void btc(x64Op src, x64CtxPo ctx);
void bsf(x64Op dst, x64Op src, x64CtxPo ctx);
void bsr(x64Op dst, x64Op src, x64CtxPo ctx);

void sete(x64Op dst, x64Op src, x64CtxPo ctx);
void setne(x64Op dst, x64Op src, x64CtxPo ctx);
void seta(x64Op dst, x64Op src, x64CtxPo ctx);
void setae(x64Op dst, x64Op src, x64CtxPo ctx);
void setg(x64Op dst, x64Op src, x64CtxPo ctx);
void setge(x64Op dst, x64Op src, x64CtxPo ctx);
void setl(x64Op dst, x64Op src, x64CtxPo ctx);
void setle(x64Op dst, x64Op src, x64CtxPo ctx);
void sets(x64Op dst, x64Op src, x64CtxPo ctx);
void setns(x64Op dst, x64Op src, x64CtxPo ctx);
void seto(x64Op dst, x64Op src, x64CtxPo ctx);
void setno(x64Op dst, x64Op src, x64CtxPo ctx);
void setpe(x64Op dst, x64Op src, x64CtxPo ctx);
void setpo(x64Op dst, x64Op src, x64CtxPo ctx);

void test(x64Op dst, x64Op src, x64CtxPo ctx);

void jmp(x64Op dst, x64Op src, x64CtxPo ctx);
void je(x64Op dst, x64Op src, x64CtxPo ctx);
void jne(x64Op dst, x64Op src, x64CtxPo ctx);
void ja(x64Op dst, x64Op src, x64CtxPo ctx);
void jae(x64Op dst, x64Op src, x64CtxPo ctx);
void jg(x64Op dst, x64Op src, x64CtxPo ctx);
void jge(x64Op dst, x64Op src, x64CtxPo ctx);
void jl(x64Op dst, x64Op src, x64CtxPo ctx);
void jle(x64Op dst, x64Op src, x64CtxPo ctx);
void jc(x64Op dst, x64Op src, x64CtxPo ctx);
void jnc(x64Op dst, x64Op src, x64CtxPo ctx);
void jo(x64Op dst, x64Op src, x64CtxPo ctx);
void jno(x64Op dst, x64Op src, x64CtxPo ctx);
void jpe(x64Op dst, x64Op src, x64CtxPo ctx);
void jpo(x64Op dst, x64Op src, x64CtxPo ctx);
void jcxz(x64Op dst, x64Op src, x64CtxPo ctx);

void loop(x64Op dst, x64Op src, x64CtxPo ctx);
void loopz(x64Op dst, x64Op src, x64CtxPo ctx);
void loopnz(x64Op dst, x64Op src, x64CtxPo ctx);

void call(x64Op src, x64CtxPo ctx);
void ret(x64CtxPo ctx);
void iret(x64CtxPo ctx);
void intrupt(x64CtxPo ctx);
void into(x64CtxPo ctx);

void bound(x64Op dst, x64Op src, x64CtxPo ctx);

void enter(x64Op dst, x64Op src, x64CtxPo ctx);
void leave(x64Op dst, x64Op src, x64CtxPo ctx);

void stc(x64CtxPo ctx);
void clc(x64CtxPo ctx);
void cmc(x64CtxPo ctx);

void std(x64CtxPo ctx);
void cld(x64CtxPo ctx);

void lahf(x64CtxPo ctx);
void sahf(x64CtxPo ctx);
void pushf(x64CtxPo ctx);
void pushfd(x64CtxPo ctx);
void popf(x64CtxPo ctx);
void popfd(x64CtxPo ctx);

void sti(x64CtxPo ctx);
void cli(x64CtxPo ctx);

void lds(x64Op dst, x64Op src, x64CtxPo ctx);
void les(x64Op dst, x64Op src, x64CtxPo ctx);
void lfs(x64Op dst, x64Op src, x64CtxPo ctx);
void lgs(x64Op dst, x64Op src, x64CtxPo ctx);
void lss(x64Op dst, x64Op src, x64CtxPo ctx);

void lea(x64Op dst, x64Op src, x64CtxPo ctx);
void nop(x64CtxPo ctx);

void xlat(x64Op dst, x64Op src, x64CtxPo ctx);
void xlatb(x64Op dst, x64Op src, x64CtxPo ctx);

void cpuid(x64Op dst, x64CtxPo ctx);
void movebe(x64Op dst, x64Op src, x64CtxPo ctx);
void prefetchw(x64Op src, x64CtxPo ctx);
void prefetchwt1(x64Op src, x64CtxPo ctx);

void clflush(x64Op src, x64CtxPo ctx);
void clflushopt(x64Op src, x64CtxPo ctx);

void xsave(x64Op dst, x64CtxPo ctx);
void xsavec(x64Op dst, x64CtxPo ctx);
void xsaveopt(x64Op dst, x64CtxPo ctx);
void xrstor(x64Op dst, x64CtxPo ctx);
void xgetbv(x64Op dst, x64CtxPo ctx);

void rdrand(x64Op dst, x64CtxPo ctx);
void rdseed(x64Op dst, x64CtxPo ctx);

#endif
