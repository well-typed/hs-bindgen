// How often an operation ran, and how long it took.
#ifndef COUNTER_H
#define COUNTER_H

#include "stopwatch.h"

struct counter {
  int count;
  stopwatch_time total;
};

#endif
