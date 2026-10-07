// A stopwatch, after rpmsw.h in RPM.
#ifndef STOPWATCH_H
#define STOPWATCH_H

// Time in microseconds
typedef unsigned long stopwatch_time;

// The time stamp is private to the library. A prescriptive binding
// specification for the module of this header (stopwatch_p.yaml) omits it.
struct stopwatch {
  unsigned long long ticks;
};

#endif
