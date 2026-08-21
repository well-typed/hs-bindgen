/* The single translation unit holding the implementation of the header-only
 * library. The bindings are generated *without* HEADER_ONLY_IMPLEMENTATION, so
 * the generated C wrapper sees the declarations only and the symbols come from
 * this object at link time.
 */

#define HEADER_ONLY_IMPLEMENTATION
#include <header_only.h>
