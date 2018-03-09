#include "config.h"
#include <ooio.h>
#include "labels.h"
#include "globals.h"
#include "errorCodes.h"

termPo eINTRUPT;
termPo eINVAL;
termPo eNOFILE;
termPo eNOTDIR;
termPo eNOTFND;
termPo eNOPERM;
termPo eIOERROR;
termPo eFAIL;

termPo falseEnum;
termPo trueEnum;
termPo voidEnum;
termPo okEnum;
termPo failEnum;
termPo eofEnum;
termPo errorLbl;

void initGlobals() {
  eINTRUPT = (termPo) declareEnum("eINTRUPT");
  eNOTDIR = (termPo) declareEnum("eNOTDIR");
  eNOFILE = (termPo) declareEnum("eNOFILE");
  eNOTFND = (termPo) declareEnum("eNOTFND");
  eINVAL = (termPo) declareEnum("eINVAL");
  eNOPERM = (termPo) declareEnum("eNOPERM");
  eFAIL = (termPo) declareEnum("eFAIL");
  eIOERROR = (termPo) declareEnum("eIOERROR");

  falseEnum = (termPo) declareEnum("core.star#false");
  trueEnum = (termPo) declareEnum("core.star#true");

  voidEnum = (termPo) declareEnum("code.star#void");

  okEnum = (termPo) declareEnum("code.star#ok");
  failEnum = (termPo) declareEnum("code.star#fail");
  eofEnum = (termPo) declareEnum("code.star#eof");
  errorLbl = (termPo) declareLbl("code.star#error", 1);
}
