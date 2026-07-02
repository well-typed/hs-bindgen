/** @file
 * Fixtures for typedef-vs-tag name clashes that hs-bindgen deliberately does
 * NOT resolve locally; they are reported as 'MangleNamesCollision' failures.
 *
 * The typedef analysis resolves clashes only when the typedef's own type
 * /syntactically references/ the eponymous tagged type (see typedef_analysis.h
 * for those cases).  Clashes that are not locally visible fall through to the
 * name-mangler's collision check, which deselects all involved declarations.
 *
 *   Case  Description                                    Collision
 *   ------------------------------------------------------------------
 *   a     Typedef type unrelated to eponymous tag         a_unrelated (struct) vs a_unrelated (typedef)
 *   a'    Function-pointer typedef, no eponymous ref      a_fun (struct) vs a_fun (typedef)
 *   b     Typedef aliases a different tag; squashing      b_clash (struct) vs b_other (struct renamed B_clash)
 *         renames that tag to the colliding name
 *
 * Note on case (c): a typedef chain such as
 *
 *   struct c_chain { int x; };
 *   typedef struct c_chain c_chain_t;
 *   typedef c_chain_t c_chain;
 *
 * does not produce a collision in practice: because c_chain_t is the sole
 * direct typedef of struct c_chain, squashing renames the struct to C_chain_t,
 * leaving C_chain free for the outer typedef.  It is therefore omitted here.
 */

// (a) Unrelated typedef sharing a tag's name: the typedef's type ('int') does
// not reference 'struct a_unrelated' at all.  Both declarations mangle to
// 'A_unrelated' and are deselected.
struct a_unrelated { int x; };
typedef int a_unrelated;

// (a') Function-pointer typedef not referencing the eponymous tag: the
// function type contains no reference to 'struct a_fun'.  Both declarations
// mangle to 'A_fun' and are deselected.
struct a_fun { int x; };
typedef void (*a_fun)(void);

// (b) Typedef aliasing a different tag ('struct b_other'), whose name collides
// with 'struct b_clash'.  The typedef analysis squashes b_clash → struct
// b_other (sole direct typedef, sole use), renaming b_other to 'B_clash'.
// This clashes with the independently existing 'struct b_clash'.  Both structs
// and the typedef are deselected.
struct b_clash { int x; };
struct b_other { int y; };
typedef struct b_other b_clash;
