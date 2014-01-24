#include "config.h"
#include "assemP.h"
#include "heapP.h"
#include "code.h"
#include "utils.h"
#include "cafeOptions.h"
#include "jitP.h"

#include <pool.h>
#include <hash.h>
#include <iostr.h>

#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static void genByte(asmCxtPo cx,byte b);
static byte r8(Register r);

static inline logical tst(short x)
{
  return !!x;
}

static void d64(asmCxtPo cxt,byte w,byte r,byte x,byte b)
{
  genByte(cxt,0x40|w<<3|r<<2|x<<1|b);
}

static inline byte rOP(byte r)
{
  return r==_NOREG;
}

static inline byte rIP(byte r)
{
  return r==(byte)_RIP;
}

static inline byte rXP(byte r)
{
  return r>0 && (r&0x0f)>7;
}

static void rexWrxb(asmCxtPo cxt,byte l,byte w,byte r,byte x,byte b)
{
  if((w|r|x|b) || l)
    d64(cxt,w,r,x,b);
}

static void rexWrx_(asmCxtPo cxt,byte l,byte w,byte r,byte x,byte mr)
{
  rexWrxb(cxt,l,w,r,x,rIP(mr)?0:rXP(mr));
}

static void rexW_x_(asmCxtPo cxt,byte l,byte w,byte r,byte x,byte mr)
{
  rexWrx_(cxt,l,w,rXP(r),x,mr);
}

static void rexQrr(asmCxtPo cxt,byte rr,byte mr)
{
  rexW_x_(cxt,0,1,rr,0,mr);
}

static void oMrm(asmCxtPo cxt,byte op,byte mo,byte r,byte m)
{
  genByte(cxt,op);
  genByte(cxt,((mo<<6)|r8(r)<<3|m));
}

static byte r8(Register r)
{
  return ((byte)r)&0x7;
}

static void movQrr(asmCxtPo cxt,Register rs,Register rd)
{
  rexQrr(cxt,(byte)rs,(byte)rd);
  oMrm(cxt,0x89,3,r8(rs),r8(rd));
}

void genByte(asmCxtPo cxt,byte b)
{
  *cxt->state.x.uc_pc++ = b & 0xff;
}
