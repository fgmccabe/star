//
// Created by Francis McCabe on 7/2/17.
//

#include <stringBuffer.h>
#include "template.h"

static retCode parseTemplateVar(strBufferPo str, ioPo plate, char **reslt, hashPo vars, strProc defltPrc, void *cl);

retCode processTemplate(ioPo out, ioPo plate, hashPo vars, strProc defltPrc, void *cl) {
  retCode ret = Ok;
  strBufferPo str = newStringBuffer();

  while (ret == Ok && isFileAtEof(plate) != Eof) {
    if (isLookingAt(plate, "#(") == Ok) {
      char *repl;
      ret = parseTemplateVar(str, plate, &repl, vars, defltPrc, cl);
      if (ret == Ok)
        ret = outStr(out, repl);
    } else if (isLookingAt(plate, "##(") == Ok) {
      char *repl;
      ret = parseTemplateVar(str, plate, &repl, vars, defltPrc, cl);
      if (ret == Ok) {
        strBufferPo replBuffer = newReadStringBuffer(repl, (long) uniStrLen(repl));
        ret = processTemplate(out, O_IO(replBuffer), vars, defltPrc, cl);
        closeIo(O_IO(replBuffer));
      }
    } else if(isLookingAt(plate,"\\#")==Ok){
      ret = outStr(out,"#");
    }
    else {
      codePoint ch;
      ret = inChar(plate, &ch);
      if (ret == Ok)
        ret = outChar(out, ch);
    }
  }

  closeIo(O_IO(str));
  return ret;
}

retCode parseTemplateVar(strBufferPo str, ioPo plate, char **reslt, hashPo vars, strProc defltPrc, void *cl) {
  codePoint ch;
  retCode ret = Ok;
  clearStrBuffer(str);
  while (ret == Ok) {
    ret = inChar(plate, &ch);
    if (ret == Ok) {
      if (ch == ')') {
        integer len;
        char *var = getTextFromBuffer(str, &len);
         *reslt = (char *) hashGet(vars, var);

        if (*reslt == NULL && defltPrc!=NULL)
          *reslt = defltPrc(var, cl);
        if(*reslt==NULL)
          return Fail;
        else
          return Ok;
      } else
        outChar(O_IO(str), ch);
    }
  }
  return ret;
}
