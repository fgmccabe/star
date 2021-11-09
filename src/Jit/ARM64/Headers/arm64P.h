//
// Created by Francis McCabe on 7/10/20.
//

#ifndef STAR_ARM64P_H
#define STAR_ARM64P_H

#include "jitP.h"
#include "arm64.h"
#include "ooio.h"
#include "array.h"

void initAssem();
void clearCodeCtxMaps(assemCtxPo ctx);

void encodeSve(assemCtxPo ctx);

#define PREL 0x0
#define ADDSUBIMM 0x2
#define ADDSUBTAG 0x3
#define LOGICALIMM 0x4
#define MOVEWIDE 0x5
#define BITFIELD 0x6
#define EXTRACE 0x7

#define X0_mask (1u)
#define X1_mask (1u<<1u)
#define X2_mask (1u<<2u)
#define X3_mask (1u<<3u)
#define X4_mask (1u<<4u)
#define X5_mask (1u<<5u)
#define X6_mask (1u<<6u)
#define X7_mask (1u<<7u)
#define X8_mask (1u<<8u)
#define X9_mask (1u<<9u)
#define X10_mask (1u<<10u)
#define X11_mask (1u<<11u)
#define X12_mask (1u<<12u)
#define X13_mask (1u<<13u)
#define X14_mask (1u<<14u)
#define X15_mask (1u<<15u)
#define X16_mask (1u<<16u)
#define X17_mask (1u<<17u)
#define X18_mask (1u<<18u)
#define X19_mask (1u<<19u)
#define X20_mask (1u<<20u)
#define X21_mask (1u<<21u)
#define X22_mask (1u<<22u)
#define X23_mask (1u<<23)
#define X24_mask (1u<<24u)
#define X25_mask (1u<<25u)
#define X26_mask (1u<<26u)
#define X27_mask (1u<<27u)
#define X28_mask (1u<<28u)
#define X29_mask (1u<<29u)
#define X30_mask (1u<<30u)
#define X31_mask (1u<<31u)

#define one_bt(X,S) (((X)&1u)<<(S))
#define two_bt(X,S) (((X)&3u)<<(S))
#define thr_bt(X,S) (((X)&0x7u)<<(S))
#define for_bt(X,S) (((X)&0xfu)<<(S))
#define fiv_bt(X,S) (((X)&0x1fu)<<(S))
#define six_bt(X,S) (((X)&0x3fu)<<(S))
#define svn_bt(X,S) (((X)&0x7fu)<<(S))
#define ayt_bt(X,S) (((X)&0xffu)<<(S))
#define nin_bt(X,S) (((X)&0x1ffu)<<(S))
#define ten_bt(X,S) (((X)&0x3ffu)<<(S))
#define elv_bt(X,S) (((X)&0x7ffu)<<(S))
#define twl_bt(X,S) (((X)&0xfffu)<<(S))
#define thi_bt(X,S) (((X)&0x1fffu)<<(S))
#define ftn_bt(X,S) (((X)&0x3fffu)<<(S))
#define fif_bt(X,S) (((X)&0x7fffu)<<(S))
#define sxt_bt(X,S) (((X)&0xffffu)<<(S))
#define snt_bt(X,S) (((X)&0x1ffffu)<<(S))
#define ntn_bt(X,S) (((X)&0x7ffffu)<<(S))
#define tsx_bt(X,S) (((X)&0x7ffffffu)<<(S))


#endif //STAR_ARM64P_H
