/* This test header contains types from the standard library that may use
 * external bindings.  See the test-pp and test-th tests.
 */

#ifndef TEST_02_H
#define TEST_02_H

#include <stdint.h>
#include <time.h>

struct event {
    uint64_t id;  // external binding in base
    char *name;
    time_t time;  // external binding in base
};

#endif // TEST_02_H
