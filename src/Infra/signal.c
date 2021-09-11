/*
  Unix Signal handling for the Star run-time engine
  Copyright (c) 2016, 2017, 2021 and beyond. Francis G. McCabe
*/
#include <stdlib.h>
#include "ooio.h"      /* Main header file */
#include "starOptions.h"
#include "engineOptions.h"
#include "signals.h"
#include "debugP.h"

/* 
 * Signal handling functions
 */


static void sig_fatal(int sig) {
  outMsg(logFile, "bus error or segmentation fault\n");
  star_exit(EXIT_FAIL);
}

/* Handle suspension reasonably ... */
static void sig_suspend(int sig) {
  if (!interactive) {
    reset_stdin();    /* Reset the standard input channel */
    raise(SIGSTOP);             /* Actually suspend */
    setup_stdin();              /* Put it back */
  } else
    raise(SIGSTOP);             /* Actually suspend */
}

static void interruptMe(int ignored);

void setupSignals(void) {
  signal(SIGBUS, sig_fatal);
  signal(SIGTSTP, sig_suspend);  /* When the user suspends a program */
  signal(SIGPIPE, SIG_IGN);  /* We not want to be interrupted by this */
  signal(SIGSEGV, sig_fatal);
  signal(SIGFPE, sig_fatal);
  signal(SIGQUIT, interruptMe);
  signal(SIGINT, interruptMe);
//  signal(SIGIO, checkIO);
}

sigset_t stopInterrupts(void)  /* stop control-C from generating a signal */
{
  sigset_t mask;
  sigset_t current;

  sigemptyset(&mask);

  sigaddset(&mask, SIGQUIT);  /* The quit signal */
  sigaddset(&mask, SIGVTALRM);  /* The virtual timer signal */
  sigaddset(&mask, SIGALRM);  /* The real timer signal */

  sigprocmask(SIG_BLOCK, &mask, &current);
  return current;
}

void startInterrupts(sigset_t blocked)  /* enable interrupts again */
{
  sigprocmask(SIG_SETMASK, &blocked, NULL);
}

/* 
 * Interrupt handling -- on a control^C we send a message to the monitor process
 */

/* Warning --- important that SIGINT is blocked during this handler */

static void interruptMe(int ignored) /* This one is invoked when user presses ^C */
{
  star_exit(EXIT_FAIL);    /* We just abort everything */
}

void star_exit(int code) {
  if (code != 0)
    outMsg(logFile, "Terminating with code %d\n", code);

  exit(code);
}
