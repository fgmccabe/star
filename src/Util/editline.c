//
// Created by Francis McCabe on 2018-12-22.
//
// hacked version of linenoise.c

#include "editline.h"
#include "unistr.h"
#include "strng.h"
#include "formio.h"
#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <file.h>
#include <assert.h>

#define DEFAULT_HISTORY_MAX_LEN 100

static CompletionCallback *completionCallback = Null;

static int getTerminalCol();

static vectorPo history = Null;
static char *historyFileName = Null;

static retCode rawInChar(char *ch);

typedef enum {
  next = 1,
  prev = -1
} historyDir;

typedef struct {
  bufferPo lineBuff;  // Line buffer
  integer firstPos;   // Nominal start of line
  integer history_index;  /* The history index we are currently editing. */
} LineState;

enum KEY_ACTION {
  CTRL_A = 1,         /* Ctrl+a */
  CTRL_B = 2,         /* Ctrl-b */
  CTRL_C = 3,         /* Ctrl-c */
  CTRL_D = 4,         /* Ctrl-d */
  CTRL_E = 5,         /* Ctrl-e */
  CTRL_F = 6,         /* Ctrl-f */
  CTRL_H = 8,         /* Ctrl-h */
  TAB = 9,            /* Tab */
  CTRL_K = 11,        /* Ctrl+k */
  CTRL_L = 12,        /* Ctrl+l */
  ENTER = 13,         /* Enter */
  CTRL_N = 14,        /* Ctrl-n */
  CTRL_P = 16,        /* Ctrl-p */
  CTRL_T = 20,        /* Ctrl-t */
  CTRL_U = 21,        /* Ctrl+u */
  CTRL_W = 23,        /* Ctrl+w */
  ESC = 27,           /* Escape */
  BACKSPACE = 127    /* Backspace */
};

static void resetAtExit(void);
static void addLineToHistory(bufferPo lineBuff);
static void refreshLine(integer firstPos, bufferPo lineBuf);
static void refreshFromText(integer firstPos, integer pos, char *content, integer size);

/* Check if the terminal name is in the list of terminals we know are
 * not able to understand basic escape sequences. */
static logical isUnsupportedTerm(void) {
  static char *unsupported_term[] = {"dumb", "cons25", "emacs"};
  char *term = getenv("TERM");

  if (term == Null)
    return False;
  for (int ix = 0; ix < NumberOf(unsupported_term); ix++)
    if (uniCmp(term, unsupported_term[ix]) == same)
      return True;
  return False;
}

/* set up raw mode */
static struct termios orig_termios; /* In order to restore at exit.*/
static logical rawMode = False; /* For atexit() function to check if restore is needed*/
static logical atexitRegistered = False;

static retCode enableRawMode(int fd) {
  struct termios raw;

  if (!isatty(STDIN_FILENO))
    return Error;

  if (tcgetattr(fd, &orig_termios) == -1)
    return Error;

  raw = orig_termios;  /* modify the original mode */
  /* input modes: no break, no CR to NL, no parity check, no strip char,
   * no start/stop output control. */
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  /* output modes - disable post processing */
  raw.c_oflag &= ~(OPOST);
  /* control modes - set 8 bit chars */
  raw.c_cflag |= (CS8);
  /* local modes - choing off, canonical off, no extended functions,
   * no signal chars (^Z,^C) */
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  /* control chars - set return condition: min number of bytes and timer.
   * We want read to return every single byte, without timeout. */
  raw.c_cc[VMIN] = 1;
  raw.c_cc[VTIME] = 0; /* 1 byte, no timer */

  if (tcsetattr(fd, TCSAFLUSH, &raw) < 0)
    return Error;
  rawMode = True;

  if (!atexitRegistered) {
    atexit(resetAtExit);
    atexitRegistered = True;
  }

  return Ok;
}

static void disableRawMode(int fd) {
  if (rawMode) {
    tcsetattr(fd, TCSAFLUSH, &orig_termios);
  }
  rawMode = False;
}

/* Clear the screen. Used to handle ctrl+l */
static void clearScreen(LineState *ls) {
  outMsg(stdOut, "\033[H\033[2J%_");
  ls->firstPos = getTerminalCol();
}

static void beep(void) {
  outMsg(stdOut, "\07%_");
}

static char completeLine(LineState *ls) {
  vectorPo completions = completionCallback(ls->lineBuff);

  char c = 0;

  if (vectIsEmpty(completions) == 0) {
    beep();
  } else {
    logical stop = False;
    size_t i = 0;

    while (!stop) {
      /* Show completion or original buffer */
      if (i < vectLength(completions)) {
        strgPo comp = O_STRG(getVectEl(completions, i));
        refreshFromText(ls->firstPos, 0, strgVal(comp), strgLen(comp));
      } else {
        refreshLine(ls->firstPos, ls->lineBuff);
      }

      if (rawInChar(&c) != Ok) {
        return -1;
      }

      switch (c) {
        case TAB:
          i = (i + 1) % (vectLength(completions) + 1);
          if (i == vectLength(completions))
            beep();
          break;
        case ESC:
          /* Re-show original buffer */
          if (i < vectLength(completions))
            refreshLine(ls->firstPos, ls->lineBuff);
          stop = True;
          break;
        default:
          /* Update buffer and return */
          if (i < vectLength(completions)) {
            stringIntoBuffer(ls->lineBuff, O_STRG(getVectEl(completions, i)));
          }
          stop = True;
          break;
      }
    }
  }

  return c;
}

void setCompletionCallback(CompletionCallback *fn) {
  completionCallback = fn;
}

static void refreshFromText(integer firstPos, integer pos, char *content, integer size) {
  outMsg(stdOut, "\r\033[%dC%S\033[0K", firstPos - 1, content, size);
  outMsg(stdOut, "\r\033[%dC%_", pos + firstPos - 1);
}

static void refreshLine(integer firstPos, bufferPo lineBuf) {
  integer buffLen;
  char *content = getTextFromBuffer(&buffLen, lineBuf);
  refreshFromText(firstPos, bufferOutPos(lineBuf), content, buffLen);
}

retCode insertChar(LineState *l, char c) {
  return insertIntoBuffer(l->lineBuff, c);
}

/* Move cursor to the left. */
void moveLeft(LineState *l) {
  if (bufferOutPos(l->lineBuff) > 0) {
    bufferBumpOutPos(l->lineBuff, prev);
    refreshLine(l->firstPos, l->lineBuff);
  }
}

/* Move cursor to the right. */
void moveRight(LineState *l) {
  bufferBumpOutPos(l->lineBuff, next);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Move cursor to the start of the line. */
void moveHome(LineState *l) {
  rewindBuffer(l->lineBuff);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Move cursor to the end of the line. */
void moveEnd(LineState *l) {
  seekBuffer(l->lineBuff, bufferLength(l->lineBuff));
  refreshLine(l->firstPos, l->lineBuff);
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */

void moveInHistory(LineState *l, historyDir dir) {
  if (vectLength(history) > 0) {
    l->history_index = clamp(0, l->history_index + dir, vectLength(history) - 1);

    strgPo entry = O_STRG(getVectEl(history, l->history_index));

    clearBuffer(l->lineBuff);
    stringIntoBuffer(l->lineBuff, entry);
    refreshLine(l->firstPos, l->lineBuff);
  }
}

/* Delete the character at the right of the cursor */
void deleteRight(LineState *l) {
  deleteFromBuffer(l->lineBuff, 1);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Backspace implementation. */
void deleteLeft(LineState *l) {
  deleteFromBuffer(l->lineBuff, -1);
  refreshLine(l->firstPos, l->lineBuff);
}

static retCode editLine(bufferPo lineBuff) {
  LineState l = {.lineBuff = lineBuff,
    .firstPos=getTerminalCol(), .history_index=vectLength(history)};

  /* Buffer starts empty. */
  clearBuffer(lineBuff);

  while (True) {
    char c;
    integer nread;
    char seq[3];

    nread = read(STDIN_FILENO, &c, 1);

    if (nread < 0) {
      switch (errno) {
        case 0:                           // Linux and/or solaris sometimes does this
          continue;
        case EINTR:
          //         return Interrupt;    // report an interrupt
        case EAGAIN:
          continue;
        default:
          return Error;
      }
    } else if (nread == 0) {
      return Ok;
    }

    switch (c) {
      case ENTER:    /* enter */
        if (appendToBuffer(lineBuff, "\n", 1) != Ok)
          return Error;

        addLineToHistory(l.lineBuff);
        return Ok;
      case TAB: {
        if (completionCallback != Null) {
          c = completeLine(&l);
          if (c < 0) {
            return Error;
          }
        } else
          beep();
        continue;
      }
      case CTRL_C:     /* ctrl-c */
        return Interrupt;
      case BACKSPACE:   /* backspace */
      case CTRL_H:
        deleteLeft(&l);
        break;
      case CTRL_D:     /* ctrl-d, remove char at right of cursor, or if the
                            line is empty, act as end-of-file. */
        if (bufferLength(l.lineBuff) > 0) {
          deleteRight(&l);
        } else {
          return Eof;
        }
        break;
      case CTRL_T:    /* ctrl-t, swaps current character with previous. */
        twizzleBuffer(l.lineBuff, bufferOutPos(l.lineBuff));
        refreshLine(l.firstPos, lineBuff);
        break;
      case CTRL_B:     /* ctrl-b */
        moveLeft(&l);
        break;
      case CTRL_F:     /* ctrl-f */
        moveRight(&l);
        break;
      case CTRL_P:    /* ctrl-p */
        moveInHistory(&l, prev);
        break;
      case CTRL_N:    /* ctrl-n */
        moveInHistory(&l, next);
        break;
      case ESC:    /* escape sequence */
        /* Read the next two bytes representing the escape sequence.
         * Use two calls to handle slow terminals returning the two
         * chars at different times. */
        if (rawInChar(&seq[0]) != Ok)
          break;
        if (rawInChar(&seq[1]) != Ok)
          break;

        /* ESC [ sequences. */
        if (seq[0] == '[') {
          if (seq[1] >= '0' && seq[1] <= '9') {
            /* Extended escape, read additional byte. */
            if (rawInChar(&seq[2]) != Ok)
              break;
            if (seq[2] == '~') {
              switch (seq[1]) {
                case '3': /* Delete key. */
                  deleteRight(&l);
                  break;
              }
            }
          } else {
            switch (seq[1]) {
              case 'A': /* Up */
                moveInHistory(&l, prev);
                break;
              case 'B': /* Down */
                moveInHistory(&l, next);
                break;
              case 'C': /* Right */
                moveRight(&l);
                break;
              case 'D': /* Left */
                moveLeft(&l);
                break;
              case 'H': /* Home */
                moveHome(&l);
                break;
              case 'F': /* End*/
                moveEnd(&l);
                break;
            }
          }
        }

          /* ESC O sequences. */
        else if (seq[0] == 'O') {
          switch (seq[1]) {
            case 'H': /* Home */
              moveHome(&l);
              break;
            case 'F': /* End*/
              moveEnd(&l);
              break;
          }
        }
        break;
      default:
        if (insertChar(&l, c) != Ok)
          return Error;
        else
          refreshLine(l.firstPos, lineBuff);
        break;
      case CTRL_U: /* Ctrl+u, delete the whole line. */
        clearBuffer(l.lineBuff);
        refreshLine(l.firstPos, lineBuff);
        break;
      case CTRL_K: /* Ctrl+k, delete from current to end of line. */
        deleteFromBuffer(l.lineBuff, bufferLength(lineBuff)-bufferOutPos(lineBuff));
        refreshLine(l.firstPos, lineBuff);
        break;
      case CTRL_A: /* Ctrl+a, go to the start of the line */
        moveHome(&l);
        break;
      case CTRL_E: /* ctrl+e, go to the end of the line */
        moveEnd(&l);
        break;
      case CTRL_L: /* ctrl+l, clear screen */
        clearScreen(&l);
        refreshLine(l.firstPos, lineBuff);
        break;
    }
  }
}

retCode consoleInput(bufferPo lineBuff) {
  if (!isatty(STDIN_FILENO) || isUnsupportedTerm()) {
    return inLine(stdIn, lineBuff, "\n");
  } else {
    if (enableRawMode(STDIN_FILENO) != Ok) {
      disableRawMode(STDIN_FILENO);
      return Error;
    }

    retCode ret = editLine(lineBuff);
    disableRawMode(STDIN_FILENO);
    outMsg(stdOut, "\n%_");

    return ret;
  }
}

/* At exit we'll try to fix the terminal to the initial conditions. */
static void resetAtExit(void) {
  disableRawMode(STDIN_FILENO);
}

static int history_max_len = DEFAULT_HISTORY_MAX_LEN;

void addLineToHistory(bufferPo lineBuff) {
  assert(history != Null);

  integer len;
  char *line = getTextFromBuffer(&len, lineBuff);

  while (len > 0 && (line[len - 1] == '\r' || line[len - 1] == '\n'))
    len--;

  if (vectLength(history) > 0) {
    strgPo last = O_STRG(getVectEl(history, vectLength(history) - 1));

    char *lastText = strgVal(last);
    integer lastLen = strgLen(last);

    if (uniNCmp(line, len, lastText, lastLen) != same && !uniIsTrivial(line, len)) {
      strgPo text = newStrng(len, line);
      appendVectEl(history, O_OBJECT(text));
    }
    if (vectLength(history) > history_max_len) {
      objectPo first = removeVectEl(history, 0);
      decReference(first);
    }
  } else {
    strgPo text = newStrng(len, line);
    appendVectEl(history, O_OBJECT(text));
  }
}

/* Load history from the history file */
retCode initHistory(char *filename) {
  historyFileName = filename;
  history = vector(0);

  atexit(saveHistory);

  ioPo historyFile = openInFile(historyFileName, utf8Encoding);
  if (historyFile != Null) {
    bufferPo lineBuffer = newStringBuffer();

    retCode ret = Ok;
    while (ret == Ok && isFileAtEof(historyFile) != Eof) {
      ret = inLine(historyFile, lineBuffer, "\n");
      if (ret == Ok) {
        if (!isTrivialBuffer(lineBuffer))
          addLineToHistory(lineBuffer);
      }
    }
    closeFile(historyFile);
    closeFile(O_IO(lineBuffer));
    return ret;
  } else
    return Ok;

}

/* Save history in the history file. */
void saveHistory() {
  if (historyFileName != Null && history != Null) {
    ioPo historyFile = newOutFile(historyFileName, utf8Encoding);
    for (integer ix = 0; ix < vectLength(history); ix++) {
      strgPo line = O_STRG(getVectEl(history, ix));
      outMsg(historyFile, "%Q\n", line);
    }

    closeFile(historyFile);
    decReference(O_OBJECT(history));
  }
}

/* Use an escape sequence to query the horizontal cursor position */
static int getTerminalCol() {
  char buf[32];
  int cols, rows;

  /* Report cursor location */
  outMsg(stdOut, "\033[6n%_");

  /* Read the response: ESC [ rows ; cols R */
  int ix = 0;
  for (; ix < NumberOf(buf); ix++) {
    if (rawInChar(&buf[ix]) != Ok)
      return -1;
    else if (buf[ix] == 'R') {
      break;
    }
  }
  buf[ix] = '\0';

  if (sscanf(buf, "\033[%d;%d", &rows, &cols) != 2)
    return -1;
  return cols;
}

retCode rawInChar(char *ch) {
  char buf[1];
  if (read(STDIN_FILENO, buf, NumberOf(buf)) != NumberOf(buf))
    return Fail;
  else {
    *ch = buf[0];
    return Ok;
  }

}
