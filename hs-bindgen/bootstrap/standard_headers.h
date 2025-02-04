#ifndef STANDARD_HEADERS_H
#define STANDARD_HEADERS_H

/**
 * C Standard Library header files
 *
 * Reference:
 *
 * * https://en.cppreference.com/w/c/header
 * * https://en.wikipedia.org/wiki/C_standard_library
 */

/**
 * C89/C90
 *
 * Reference:
 *
 * * https://web.archive.org/web/20161223125339/http://flash-gordon.me.uk/ansi.c.txt
 *   4.1.2 Standard headers
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/**
* C95 (Normative Addendum 1)
*
* Reference:
*
* * http://www.lysator.liu.se/c/na1.html
*/

#include <iso646.h>
#include <wchar.h>
#include <wctype.h>

/**
 * C99
 *
 * Reference:
 *
 * * https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
 *   7.1.2 Standard headers
 *
 * Notes:
 *
 * * complex.h is optional
 * * stdbool.h is deprecated in C23
 */

#include <complex.h>
#include <fenv.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <tgmath.h>

/**
 * C11
 *
 * Reference:
 *
 * * https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
 *   7.1.2 Standard headers
 *
 * Notes:
 *
 * * stdalign.h is deprecated in C23
 * * stdatomic.h is optional, and it is not included in musl
 * * stdnoreturn.h is deprecated in C23
 * * threads.h is optional
 */

#include <stdalign.h>
// #include <stdatomic.h>
#include <stdnoreturn.h>
#include <threads.h>
#include <uchar.h>

/**
 * C17
 *
 * Reference:
 *
 * * https://web.archive.org/web/20181230041359/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf
 *   7.1.2 Standard headers
 */

/**
 * C23
 *
 * Reference:
 *
 * * https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3096.pdf
 *   7.1.2 Standard headers
 *
 * Notes:
 *
 * * stdbit.h is not included in musl
 * * stdckdint.h is not included in musl
 */

// #include <stdbit.h>
// #include <stdckdint.h>

#endif
