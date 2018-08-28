//
// Created by Francis McCabe on 8/25/18.
// Support console history

#ifndef CAFE_HISTORY_H
#define CAFE_HISTORY_H

#include "file.h"
#include "strng.h"

void initHistory(char *historyFileName);
integer historyLength();    // How many line of history do we have?
strgPo fetchHistory(integer ix);
retCode appendHistoryLine(char *txt, integer len);


#endif //CAFE_HISTORY_H
