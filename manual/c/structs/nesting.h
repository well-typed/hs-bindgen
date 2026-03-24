/* -------------------------------------------------------------------------- */
/* Example A */

struct doorA {
  float height;
  float width;
};

struct roomA {
  struct doorA door1;
  struct doorA door2;
};

/* -------------------------------------------------------------------------- */
/* Example B */

struct roomB {
  struct doorB {
    float height;
    float width;
  };
  struct doorB door1;
  struct doorB door2;
};

/* -------------------------------------------------------------------------- */
/* Example C */

struct roomC {
  struct doorC {
    float height;
    float width;
  } door1;
  struct doorC door2;
};

/* -------------------------------------------------------------------------- */
/* Example D */

struct roomD {
  struct {
    float height;
    float width;
  } door1;
};

/* -------------------------------------------------------------------------- */
/* Example E */

struct roomE {
  struct {
    float height;
    float width;
  };
};

