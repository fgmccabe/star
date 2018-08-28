//
// Created by Francis McCabe on 8/25/18.
//

#include <assert.h>
#include "history.h"
#include "fileP.h"
#include "vector.h"

static vectorPo history;
static integer currHistoryLineNo = 0;
static ioPo historyFile;

void initHistory(char *historyFileName) {
  historyFile = openInOutFile(historyFileName, utf8Encoding);
  history = vector(32);
  bufferPo lineBuffer = newStringBuffer();

  fileSeek(O_FILE(historyFile), 0);

  retCode ret = Ok;

  while (ret == Ok && isFileAtEof(historyFile) != Eof) {
    ret = inLine(historyFile, lineBuffer, "\n");
    if (ret == Ok) {
      integer lineLength;
      char *lineText = getTextFromBuffer(&lineLength, lineBuffer);
      strgPo line = newStrng(lineLength, lineText);
      appendVectEl(history, O_OBJECT(line));
      currHistoryLineNo = vectLength(history);
    }
  }
  closeFile(O_IO(lineBuffer));
}

integer historyLength(){
  return vectLength(history);
}

strgPo fetchHistory(integer ix) {
  assert(ix >= 0 && ix < vectLength(history));
  return O_STRG(getVectEl(history, ix));
}

retCode appendHistoryLine(char *txt, integer len) {
  strgPo line = newStrng(len, txt);
  appendVectEl(history, O_OBJECT(line));

  assert(fileOutReady(historyFile)==Ok);
  retCode ret = outText(historyFile, txt, len);
  if (ret == Ok )
    flushFile(historyFile);
  return ret;
}



