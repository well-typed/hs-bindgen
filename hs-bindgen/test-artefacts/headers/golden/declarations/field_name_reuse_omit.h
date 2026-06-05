/** @file
 * Field-selector names reused by non-field declarations, under OmitFieldPrefixes.
 *
 * Under OmitFieldPrefixes the generated module enables both
 * DuplicateRecordFields and NoFieldSelectors.  Because no top-level field
 * selector is generated, an unprefixed field name can freely coincide with a
 * non-field declaration of the same name; fields are reached through HasField
 * (record-dot) instead.  None of the declarations below collide, and all are
 * generated.
 *
 * (Two records merely sharing a field name -- DuplicateRecordFields -- is
 * exercised elsewhere; here the point is field-vs-non-field.)
 *
 *   Case  Description                          Shared Haskell variable name
 *   ---------------------------------------------------------------------------
 *   a     Field selector and global variable   struct S.foo and global foo
 *   b     Field selector and function          struct T.bar and function bar
 */

// (a) The field 'foo' of 'struct S' and the global variable 'foo' share the
// Haskell variable name 'foo'; with NoFieldSelectors there is no clash.
struct S { int foo; };
extern int foo;

// (b) The field 'bar' of 'struct T' and the function 'bar' share the Haskell
// variable name 'bar'; again no clash.
struct T { int bar; };
void bar(void);
