#ifndef _ESCODES_H_
#define _ESCODES_H_

#undef escape
#define escape(name,spec,cmnt) \
 Esc##name,


#define escapeOpCode(name) (Esc##name)

typedef enum {
#include "escapes.h"
  Esc_None
} EscapeCode;

#endif
