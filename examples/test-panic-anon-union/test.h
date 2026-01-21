/**
 * Minimal reproduction test for anonymous union panic
 *
 * This file contains the smallest C code pattern that triggers the panic:
 * "Unexpected multiple locations for anon decl"
 *
 * The bug occurs when:
 * 1. A struct (outer) contains a field (u) of anonymous union type
 * 2. That struct is then used as a field type in another struct (inner)
 * 3. An external binding spec references the anonymous union
 */

// First header: Defines a struct with an anonymous union field
// When processed with --gen-binding-spec, this creates a reference like:
//   cname: union @outer_u
struct outer {
    union {        // Anonymous union - no tag name
        int x;     // Member 1
        int y;     // Member 2
    } u;           // Field named 'u' of anonymous union type
};

// Second header: Uses the struct as a field type
// When processed with --external-binding-spec pointing to the first
// header's binding spec, the code tries to look up locations for
// 'outer_u' and finds empty locations [], triggering the panic.
struct inner {
    struct outer field;  // This indirectly references the anonymous union
};
