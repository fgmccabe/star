/*
  Signal management functions
  Copyright (c) 2016, 2017, 2022 and beyond. Francis G. McCabe

*/
#ifndef _G_SIGNAL_H_
#define _G_SIGNAL_H_

#include <signal.h>

void setupSignals(void);
void setupSimpleHandler(int signal, void (*handler)(int));
void setupIOHandler(int signal, void (*handler)(int, siginfo_t *, void *));

void startInterrupts(sigset_t blocked);  /* enable control-C interrupts */
sigset_t stopInterrupts(void);  /* stop control-C interruptes */
void star_exit(int);    /* When we want to stop */
void initSuspend(void);
#endif
