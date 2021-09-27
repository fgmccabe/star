/*
  Return code enumeration
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
*/

#ifndef RET_CODE_H_
#define RET_CODE_H_

typedef enum {
  Ok, Fail, Switch, Suspend, Interrupt, Error, Eof, Space
} retCode;

#define MAX_RETCODE ((int)Space)

typedef enum {
  smaller, /* One item is smaller than another */
  same, /* Two items are the same */
  bigger, /* One it bigger than the other */
  incomparible                  /* Incomparible */
} comparison;

#define tryRet(Exp) STMT_WRAP({ retCode ret=(Exp); if(ret!=Ok)return ret; })

#endif
