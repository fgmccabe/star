//
// Created by Francis McCabe on 8/25/18.
//

#include "file.h"
#include "lineEdit.h"
#include "stringBuffer.h"
#include "history.h"

typedef retCode (*editCmd)(codePoint ch, void *cl);

typedef struct {
  codePoint c;
  char *usage;
  void *cl;
  editCmd cmd;
} EditCmd, *editOptPo;

static bufferPo currLine = Null;

static retCode endLine(codePoint ch, void *cl);

EditCmd commands[] = {
  {.c = '\n', .usage="complete line", .cl=Null, .cmd=endLine}
};

retCode consoleInput(char *buffer, integer buffLen, integer *read) {
  if (currLine == Null)
    currLine = newIoStringBuffer();

  retCode ret = Ok;

  if (isFileAtEof(O_IO(currLine)) == Eof) {
    clearBuffer(currLine);
    integer currPos = 0;

    while (ret == Ok) {
      codePoint ch;
      ret = inChar(rawStdIn, &ch);

      if (ret == Ok) {
        for (integer ix = 0; ix < NumberOf(commands); ix++) {
          editOptPo editCmd = &commands[ix];
          if (editCmd->c == ch) {
            ret = editCmd->cmd(ch, editCmd->cl);
            if (ret == Eof) {
              return inBytes(O_IO(currLine), (byte *) buffer, buffLen, read);
            } else
              goto contLoop;
          }
        }
        ret = insertIntoBuffer(currLine, ch, &currPos);
      }
      contLoop:;
    }
  }

  return inBytes(O_IO(currLine), (byte *) buffer, buffLen, read);
}

retCode endLine(codePoint ch, void *cl) {
  outChar(O_IO(currLine), '\n');
  integer lineLen;
  char *lineTxt = getTextFromBuffer(&lineLen, currLine);
  appendHistoryLine(lineTxt, lineLen);

  return Eof;     // Signal end of editing
}
