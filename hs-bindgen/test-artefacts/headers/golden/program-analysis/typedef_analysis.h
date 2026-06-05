
/** @file
 * Examples for the various cases in `HsBindgen.Frontend.Analysis.Typedefs`
 *
 * A C typedef and a tagged type (struct/union/enum) can share a name because
 * they live in different C namespaces, but they collide in Haskell. The typedef
 * analysis resolves this locally -- using only a single declaration's own name
 * and type structure -- and so can only act on clashes that are visible from
 * the typedef itself. Clashes that are not locally visible are detected and
 * reported as failures by the name mangler instead (see the companion fixture
 * for those).
 *
 *   Form of clash                                     Example  Conclusion
 *   -------------------------------------------------------------------------
 *   Typedef is a direct alias, sole use               1,3,4,10 squash
 *   Typedef is a direct alias, declared together      2        squash
 *   Typedef is a direct alias, same name              8,9      squash
 *   Typedef is a direct alias, self-referential       11,12    squash
 *   Direct alias (const-qualified), same name         15       squash
 *   Direct alias used by more than one typedef        7        keep both
 *   Indirection, name mismatch                        5        keep both
 *   Eponymous tag via pointer                         6a       suffix tag
 *   Eponymous tag via array                           6b       suffix tag
 *   Eponymous tag via function return type            13       suffix tag
 *   Eponymous tag via function argument type          14       suffix tag
 *   Eponymous tag via const pointer                   16       suffix tag
 *   Eponymous tag via block return type               blk*     suffix tag
 *   Eponymous tag via block argument type             blkarg*  suffix tag
 *
 * (*) Block examples are in typedef_block.h (requires -fblocks).
 *
 * A type qualifier (`const`) is transparent for this analysis: it changes
 * neither the Haskell representation nor the mangled name, so the tagged type
 * underneath it keeps its directness (examples 15 and 16).
 *
 * The suffix added to a tagged type mirrors its C tag keyword: `_struct`,
 * `_union`, or `_enum`.
 */

// Example 1: struct only used in typedef
struct struct1 {};
typedef struct struct1 struct1_t;

// Example 2: like example 1, but declared together
typedef struct struct2 {
} struct2_t;

// Example 3: like example1, but opaque struct
struct struct3;
typedef struct struct3 struct3_t;

// Example 4: like example2, but opaque struct
typedef struct struct4 struct4_t;

// Example 5: intervening pointer
struct struct5 {};
typedef struct struct5 *struct5_t;

// Example 6a: typedef pointer to eponymous struct
typedef struct struct6a {
} *struct6a;

// Example 6b: like example 6a, but an array rather than a pointer
//
// See <https://github.com/well-typed/hs-bindgen/issues/1445>: as with the
// pointer case, the eponymous struct must be assigned a new name (suffixed)
// rather than clashing with the typedef.
typedef struct struct6b {
} struct6b[50];

// Example 7: struct used in more than one typedef
struct struct7 {};
typedef struct struct7 struct7a;
typedef struct struct7 struct7b;

// Example 8: used in more than one typedef, but one has same name
struct struct8 {};
typedef struct struct8 struct8;
typedef struct struct8 struct8b;

// Example 9: typedef of a typedef, first typedef eliminated (same name)
struct struct9 {};
typedef struct struct9 struct9;
typedef struct9 struct9_t;

// Example 10: like example9, but typedef eliminated because only use of struct
//
// This example shows that we are interested in /direct/ uses of a struct in
// in order to determine whether or not to eliminate the typedef.
struct struct10 {};
typedef struct struct10 struct10_t;
typedef struct10_t struct10_t_t;

// Example 11: struct only used by /itself/ and a typedef
struct struct11 {
  int x;
  struct struct11 *self;
};
typedef struct struct11 struct11_t;

// Example 12: like example 11, but with forward declaration
struct struct12;
typedef struct struct12 struct12_t;
struct struct12 {
  int x;
  struct12_t *self;
};

// Use sites (to verify that these are also handlded correctly)
struct use_sites {
  // no use of struct1 (otherwise the heuristic would no longer apply)
  struct1_t useTypedef_struct1_t;

  // similarly for struct2
  struct2_t useTypedef_struct2_t;

  // similarly for struct3 and struct4, but must be pointers (opaque types)
  struct3_t *useTypedef_struct3_t;
  struct4_t *useTypedef_struct4_t;

  // for struct5 both types continue to exist
  struct struct5 useStruct_struct5;
  struct5_t useTypedef_struct5_t;

  // similarly for struct6a, except here the Haskell type is assigned a new name
  struct struct6a useStruct_struct6a;
  struct6a useTypedef_struct6a;

  // similarly for struct6b (array typedef): the struct gets a new name
  struct struct6b useStruct_struct6b;
  struct6b useTypedef_struct6b;

  // for struct7a and struct7b there are multiple typedefs, so not squashed
  struct7a useTypedef_struct7a;
  struct7b useTypedef_struct7b;

  // for struct8 and struct8a, one has the same name so is squashed, the other
  // is kept
  struct8 useTypedef_struct8;
  struct8b useTypedef_struct8b;

  // for struct9 and struct10, the typedef is squashed, but the typedef /of/
  // the typedef is not
  struct9 useTypedef_struct9;
  struct9_t useTypedef_struct9_t;
  struct10_t useTypedef_struct10_t;
  struct10_t_t useTypedef_struct10_t_t;

  // For struct11 and struct12 the typedef is squashed (despite self reference)
  struct11_t useTypedef_struct11_t;
  struct12_t useTypedef_struct12_t;
};

// Mind bending self-referential definitions.
//
// A typedef and a tagged type can share a name (they live in different C
// namespaces), but they clash in Haskell. We can resolve such a clash locally
// (suffixing the tagged type) precisely when the typedef's own type references
// the eponymous tagged type, regardless of /where/ in the type it appears.

// Example 13: the typedef references the eponymous struct in its return type;
// 'struct foo' is suffixed.
struct foo {
  int x;
  int y;
};
typedef struct foo (*foo)(void);

// Example 14: the typedef references the eponymous struct in an argument type;
// 'struct bar' is suffixed.
struct bar {
  int a;
  int b;
};
typedef struct foo (*bar)(struct bar arg);

// Example 15: const-qualified direct alias with the same name as its tag.
//
// The qualifier sits at the head of the type (a 'TypeQual'), but it is
// transparent: 'struct struct15' is still reached /directly/, so -- exactly as
// in the unqualified same-name case (example 8) -- the typedef is squashed.
struct struct15 {};
typedef const struct struct15 struct15;

// Example 16: eponymous tag reached through a const pointer.
//
// Here the head of the type is a qualifier wrapping a pointer
// ('const' applies to the pointer itself). We must recurse /through/ the
// qualifier to discover 'struct struct16' behind the indirection; because it is
// no longer direct and clashes with the typedef name, the tag is suffixed.
struct struct16 {};
typedef struct struct16 *const struct16;

// Use sites for the qualified examples.
struct use_sites_qual {
  // struct15 is squashed, so only the typedef name survives
  struct15 useTypedef_struct15;

  // for struct16 both types continue to exist (the tag is suffixed)
  struct struct16 useStruct_struct16;
  struct16 useTypedef_struct16;
};
