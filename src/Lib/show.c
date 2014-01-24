#include "config.h"
#include "ooio.h"

#include "escapes.h"
#include "libNames.h"

static uint64 showInt(uint64 *tos)
{
  uint64 Tx = *tos;
  outMsg(logFile,"%ld\n",(int64)Tx);
  return Tx;
}

static uniChar showName[] = { 's', 'h', 'o', 'w', 0 };
static uniChar showSig[] = { escSig, '(', 'i', ')', 'i', 0 };
static EscapeRec escapeShow = {
  .name = showName,
  .sig = showSig,
  .esc = showInt,
  .arity = 0
};

void installShow()
{
  installEscape("show",&escapeShow);
}

