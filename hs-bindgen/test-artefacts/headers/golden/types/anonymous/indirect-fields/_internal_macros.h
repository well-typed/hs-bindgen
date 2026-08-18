#pragma once

/* -------------------------------------------------------------------------- */
/* macros for testing indirect fields */

// This header is not intended to be a test itself. We use it to generate types
// in headers that are tested (see other headers in this directory).

#define MkSingleNested(t1, t2, f) \
  t1 { t2 { f; }; };

#define MkMultiNested(t1, t2, t3, f) \
  t1 { t2 { t3 { f; }; }; };

// NOTE: indirect fields are handled the same for structs as for unions, so it
// does not matter whether we nest structs or unions or a combination of them.
// We arbitrarily decided to use structs in our test headers here.

#define MkSingleNestedS(f) \
  MkSingleNested(struct S, struct, f)

#define MkMultiNestedS(f) \
  MkMultiNested (struct S, struct, struct, f)
