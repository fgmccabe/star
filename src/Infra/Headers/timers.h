//
// Created by Francis McCabe on 4/16/23.
//

#ifndef STAR_TIMERS_H
#define STAR_TIMERS_H

typedef struct timer_record *timerPo;

extern logical enableTimers;

#define startTimer(Msg) (enableTimers ? startTimer_(Msg): Null)
#define pauseTimer(t) STMT_WRAP(if(enableTimers) { pauseTimer_(t);})
#define resumeTimer(t) STMT_WRAP(if(enableTimers) { resumeTimer_(t);})

timerPo startTimer_(char *msg);
void pauseTimer_(timerPo timer);
void resumeTimer_(timerPo timer);
logical isTimerRunning(timerPo timer);

void initTimers();
void reportTimers();

#endif //STAR_TIMERS_H
