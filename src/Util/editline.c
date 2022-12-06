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

static CompletionCallback completionCallback = Null;
static void *completionCl = Null;

static int getTerminalCol();

static vectorPo history = Null;
static char *historyFileName = Null;

static retCode rawInChar(char *ch);

typedef enum {
  next = 1,
  prev = -1
} historyDir;

typedef struct {
  strBufferPo lineBuff;  // Line buffer
  integer firstPos;   // Nominal start of line
  integer history_index;  /* The history index we are currently editing. */
} LineState;

enum KEY_ACTION {
  CTRL_A = CTRL((unsigned) 'A'),
  CTRL_B = CTRL((unsigned) 'B'),
  CTRL_C = CTRL((unsigned) 'C'),
  CTRL_D = CTRL((unsigned) 'D'),
  CTRL_E = CTRL((unsigned) 'E'),
  CTRL_F = CTRL((unsigned) 'F'),
  CTRL_H = CTRL((unsigned) 'H'),
  TAB = '\t',            /* Tab */
  CTRL_K = CTRL((unsigned) 'K'),
  CTRL_L = CTRL((unsigned) 'L'),
  ENTER = 10,         /* Enter */
  CTRL_N = CTRL((unsigned) 'N'),
  CTRL_P = CTRL((unsigned) 'P'),
  CTRL_T = CTRL((unsigned) 'T'),
  CTRL_U = CTRL((unsigned) 'U'),
  ESC = 27,           /* Escape */
  SPACE = ' ',         // space
  BACKSPACE = 127    /* Backspace */
};

static void resetAtExit(void);
static void addLineToHistory(strBufferPo lineBuff);
static void refreshLine(integer firstPos, strBufferPo lineBuf);
static void refreshFromText(integer firstPos, integer pos, char *content, integer size);

/* Check if the terminal name is in the list of terminals we know are
 * not able to understand basic escape sequences. */
static logical isUnsupportedTerm(void) {
  static char *unsupported_term[] = {"dumb", "cons25", "emacs"};
  char *term = getenv("TERM");

  if (term == Null)
    return True;
  for (int ix = 0; ix < NumberOf(unsupported_term); ix++)
    if (uniCmp(term, unsupported_term[ix]) == same)
      return True;
  return False;
}

/* set up raw mode */
static struct termios saved_termios; /* In order to restore at exit.*/
static logical rawMode = False;
static logical atexitRegistered = False;

static retCode enableRawMode() {
  struct termios raw;

  if (!isatty(STDIN_FILENO))
    return Error;

  if (tcgetattr(STDIN_FILENO, &saved_termios) == -1)
    return Error;

  raw = saved_termios;

  raw.c_iflag &= ~(BRKINT | INPCK | ISTRIP | IXON | ICRNL);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

  raw.c_cc[VMIN] = 1;
  raw.c_cc[VTIME] = 0; /* 1 byte, no timer */

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0)
    return Error;
  rawMode = True;

  if (!atexitRegistered) {
    atexit(resetAtExit);
    atexitRegistered = True;
  }

  return Ok;
}

static void disableRawMode() {
  if (rawMode) {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &saved_termios);
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

static void resetBuffer(strBufferPo b, strgPo old, integer pos) {
  clearStrBuffer(b);
  stringIntoStrBuffer(b, old);
  seekStrBuffer(b, pos);
}

static retCode completeLine(LineState *ls) {
  integer cx = 0;
  strgPo snapShot = stringFromBuffer(ls->lineBuff);
  integer snapPos = strBufferOutPos(ls->lineBuff);

  do {
    retCode ret = completionCallback(ls->lineBuff, completionCl, cx++);

    switch (ret) {
      case Ok: {
        char ch;
        if (rawInChar(&ch) != Ok) {
          resetBuffer(ls->lineBuff, snapShot, snapPos);
          decReference(O_OBJECT(snapShot));
          return Error;
        } else {
          switch (ch) {
            case TAB:
              resetBuffer(ls->lineBuff, snapShot, snapPos);
              continue;
            case ESC:
              resetBuffer(ls->lineBuff, snapShot, snapPos);
              decReference(O_OBJECT(snapShot));
              refreshLine(ls->firstPos, ls->lineBuff);
              return Ok;
            case SPACE:
            case ENTER:
              decReference(O_OBJECT(snapShot));
              return Ok;
            default:
              beep();
              continue;
          }
        }
      }

      case Eof:
        resetBuffer(ls->lineBuff, snapShot, snapPos);
        decReference(O_OBJECT(snapShot));
        refreshLine(ls->firstPos, ls->lineBuff);
        return Ok;
      default:
      case Error:
        return Error;
    }
  } while (True);
}

void setEditLineCompletionCallback(CompletionCallback fn, void *cl) {
  completionCallback = fn;
  completionCl = cl;
}

void clearEditLineCompletionCallback() {
  completionCallback = Null;
  completionCl = Null;
}

static void refreshFromText(integer firstPos, integer pos, char *content, integer size) {
  outMsg(stdOut, "\r\033[%dC%S\033[0K", firstPos - 1, content, size);
  outMsg(stdOut, "\r\033[%dC%_", pos + firstPos - 1);
}

static void refreshLine(integer firstPos, strBufferPo lineBuf) {
  integer buffLen;
  char *content = getTextFromBuffer(lineBuf, &buffLen);
  refreshFromText(firstPos, strBufferOutPos(lineBuf), content, buffLen);
}

retCode insertChar(LineState *l, char c) {
  return insertIntoStringBuffer(l->lineBuff, (codePoint) c);
}

/* Move cursor to the left. */
void moveLeft(LineState *l) {
  if (strBufferOutPos(l->lineBuff) > 0) {
    strBufferBumpOutPos(l->lineBuff, prev);
    refreshLine(l->firstPos, l->lineBuff);
  }
}

/* Move cursor to the right. */
void moveRight(LineState *l) {
  strBufferBumpOutPos(l->lineBuff, next);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Move cursor to the start of the line. */
void moveHome(LineState *l) {
  rewindStrBuffer(l->lineBuff);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Move cursor to the end of the line. */
void moveEnd(LineState *l) {
  seekStrBuffer(l->lineBuff, strBufferLength(l->lineBuff));
  refreshLine(l->firstPos, l->lineBuff);
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */

void moveInHistory(LineState *l, historyDir dir) {
  if (vectLength(history) > 0) {
    l->history_index = clamp(0, l->history_index + dir, vectLength(history) - 1);

    strgPo entry = O_STRG(getVectEl(history, l->history_index));

    clearStrBuffer(l->lineBuff);
    stringIntoStrBuffer(l->lineBuff, entry);
    refreshLine(l->firstPos, l->lineBuff);
  }
}

/* Delete the character at the right of the cursor */
void deleteRight(LineState *l) {
  deleteFromStrBuffer(l->lineBuff, 1);
  refreshLine(l->firstPos, l->lineBuff);
}

/* Backspace implementation. */
void deleteLeft(LineState *l) {
  deleteFromStrBuffer(l->lineBuff, -1);
  refreshLine(l->firstPos, l->lineBuff);
}

static retCode editLine(strBufferPo lineBuff) {
  LineState l = {.lineBuff = lineBuff,
    .firstPos=getTerminalCol(), .history_index=vectLength(history)};

  /* Buffer starts empty. */
  clearStrBuffer(lineBuff);

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
          return Interrupt;               // report an interrupt
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
        if (appendToStrBuffer(lineBuff, "\n", 1) != Ok)
          return Error;

        addLineToHistory(lineBuff);
        return Ok;
      case TAB: {
        if (completionCallback != Null) {
          if (completeLine(&l) != Ok)
            return Error;
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
        if (strBufferLength(lineBuff) > 0) {
          deleteRight(&l);
        } else {
          return Eof;
        }
        break;
      case CTRL_T:    /* ctrl-t, swaps current character with previous. */
        twizzleStrBuffer(lineBuff, strBufferOutPos(lineBuff));
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
        clearStrBuffer(l.lineBuff);
        refreshLine(l.firstPos, lineBuff);
        break;
      case CTRL_K: /* Ctrl+k, delete from current to end of line. */
        deleteFromStrBuffer(l.lineBuff, strBufferLength(lineBuff) - strBufferOutPos(lineBuff));
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

retCode consoleInput(strBufferPo lineBuff) {
  if (!isatty(STDIN_FILENO) || isUnsupportedTerm()) {
    return inLine(stdIn, lineBuff, "\n");
  } else if (enableRawMode() != Ok) {
    disableRawMode();
    return inLine(stdIn, lineBuff, "\n");
  } else {
    retCode ret = editLine(lineBuff);
    disableRawMode();
    outMsg(stdOut, "\n%_");

    return ret;
  }
}

/* At exit we'll try to fix the terminal to the initial conditions. */
static void resetAtExit(void) {
  disableRawMode();
}

static int history_max_len = DEFAULT_HISTORY_MAX_LEN;

void addLineToHistory(strBufferPo lineBuff) {
  assert(history != Null);

  integer len;
  char *line = getTextFromBuffer(lineBuff, &len);

  if (len > 1) {
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
}

static void saveHistory();

/* Load history from the history file */
retCode initHistory(char *filename) {
  historyFileName = filename;
  history = vector(0);

  if (filename != Null) {
    atexit(saveHistory);

    ioPo historyFile = openInFile(historyFileName, utf8Encoding);
    if (historyFile != Null) {
      strBufferPo lineBuffer = newStringBuffer();

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
    }
  }
  return Ok;
}

/* Save history in the history file. */
void saveHistory() {
  if (historyFileName != Null && history != Null && !vectIsEmpty(history)) {
    ioPo historyFile = newOutFile(historyFileName, utf8Encoding);
    for (integer ix = 0; ix < vectLength(history); ix++) {
      strgPo line = O_STRG(getVectEl(history, ix));
      outMsg(historyFile, "%U\n", line);
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
