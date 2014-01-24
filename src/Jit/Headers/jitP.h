#ifndef _JIT_P_H_
#define _JIT_P_H_

#include "lightning.h"

typedef enum { counting, generating } codeGenMode;

typedef struct _assemble_context_ {
  void *buffer;
  long size;				/* current size of the buffer */
  long pc;
  long argDepth;			/* how many arguments are there */
  jit_state state;
  codeGenMode mode;
  hashPo lbls;				/* table of labels and addresses */
  methodPo mtd;				/* what method are we generating? */
} AssembleContext, *asmCxtPo;

#endif
