/*
  Unix Signal handling for the Star run-time engine
  Copyright (c) 2016, 2017, 2021 and beyond. Francis G. McCabe
*/
#include <stdlib.h>
#include "ooio.h"      /* Main header file */
#include "signals.h"

/* 
 * Signal handling functions
 */


static volatile sig_atomic_t gotSIGQUIT = 0;
static volatile sig_atomic_t gotControlC = 0;

static void quitHandler(int sig) {
  gotSIGQUIT = 1;
}

#define IO_SIGNAL SIGUSR1       // We will use this to signal I/O events

static void aioSigHandler(int sig, siginfo_t *si, void *cl) {
  if (si->si_code == SI_ASYNCIO) {
    logMsg(logFile, "We have an I/O completion signal");
  }
}

static void busErrorHandler(int sig) {
  syserr("bus error or segmentation fault");
}

static void interruptMe(int ignored) /* This one is invoked when user presses ^C */
{
  gotControlC = 1;
  syserr("control-C interrupt");    /* We just abort everything */
}

logical controlC() {
  return gotControlC == 1;
}

void setupSimpleHandler(int signal, void (*handler)(int)) {
  struct sigaction sa;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  sa.sa_handler = handler;
  if (sigaction(signal, &sa, Null) == -1)
    syserr("error in setting up simple handler");
}

void setupIOHandler(int signal, void (*handler)(int, siginfo_t *, void *)) {
  struct sigaction sa;
  sa.sa_flags = SA_RESTART | SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  if (sigaction(signal, &sa, Null) == -1)
    syserr("error in setting up I/O handler");
}

void setupSignals() {
  setupSimpleHandler(SIGQUIT, quitHandler);
  setupSimpleHandler(SIGINT, interruptMe);
  setupSimpleHandler(SIGBUS, busErrorHandler);
  setupSimpleHandler(SIGSEGV, busErrorHandler);
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
