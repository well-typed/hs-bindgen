
/**
 * Examples for the various cases in by `HsBindgen.Frontend.Analysis.Typedefs`
 */

// Example 1: struct only used in typedef
struct struct1 {};
typedef struct struct1 struct1_t;

// Example 2: like example 1, but declared together
typedef struct struct2 {} struct2_t;

// Example 3: like example1, but opaque struct
struct struct3;
typedef struct struct3 struct3_t;

// Example 4: like example2, but opaque struct
typedef struct struct4 struct4_t;

// Example 5: intervening pointer
struct struct5 {};
typedef struct struct5 *struct5_t;

// Example 6: typedef pointer to eponymous struct
typedef struct struct6 {} *struct6;

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
  struct3_t* useTypedef_struct3_t;
  struct4_t* useTypedef_struct4_t;

  // for struct5 both types continue to exist
  struct struct5 useStruct_struct5;
  struct5_t useTypedef_struct5_t;

  // similarly for struct6, except here the Haskell type is assigned a new name
  struct struct6 useStruct_struct6;
  struct6 useTypedef_struct6;

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

