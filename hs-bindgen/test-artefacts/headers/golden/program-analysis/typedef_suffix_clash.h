/** @file
 * The deterministic tag suffix (`_struct`/`_union`/`_enum`; see
 * typedef_analysis.h) is never counter-disambiguated, so it can collide with an
 * independently-named declaration. When it does, the name-mangler deselects
 * /both/ colliding declarations rather than inventing a `_struct2`.
 */

// (sfx) 'struct sfx' is reached through a pointer by the eponymous typedef
// 'sfx', so it is suffixed to 'Sfx_struct'.  A real 'struct sfx_struct' also
// mangles to 'Sfx_struct'.  Both structs are deselected; the typedef 'sfx'
// then loses its (now dropped) dependency.
struct sfx {
  int x;
};
typedef struct sfx *sfx;
struct sfx_struct {
  int y;
};

// (u) The same, one namespace over: 'union u' is suffixed to 'U_union', which
// collides with a real 'struct u_union'.
union u {
  int a;
};
typedef union u *u;
struct u_union {
  int b;
};
