/*
 * Header file for defining unicode characteristics 
 */
 
 #ifndef _UNICHAR_H_
 #define _UNICHAR_H_
 
 typedef enum{
  Cc,  Cf,  Cn,  Co,  Cs,
  Ll,  Lm,  Lo,  Lt,  Lu,
  Mc,  Me,  Mn,
  Nd,  Nl,  No,
  Pc,  Pd,  Pe,  Pf,  Pi,  Po,  Ps,
  Sc,  Sk,  Sm,  So,
  Zl,  Zp,  Zs, Other
} UniCharCatagory;

extern UniCharCatagory genCatTbl[];

#define MAXUNICODE (1<<16)

#endif
