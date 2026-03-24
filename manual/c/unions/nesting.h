/* -------------------------------------------------------------------------- */
/* Preliminaries */

enum shape_tag { circle, square };

/* -------------------------------------------------------------------------- */
/* Example A */

union sizeA {
  float radius;
  int length;
};

struct shapeA {
  enum shape_tag tag;
  union sizeA size;
};

/* -------------------------------------------------------------------------- */
/* Example B */

struct shapeB {
  enum shape_tag tag;
  union sizeB {
    float radius;
    int length;
  };
  union sizeB size;
};

/* -------------------------------------------------------------------------- */
/* Example C */

struct shapeC {
  enum shape_tag tag;
  union sizeC {
    float radius;
    int length;
  } size;
};

/* -------------------------------------------------------------------------- */
/* Example D */

struct shapeD {
  enum shape_tag tag;
  union {
    float radius;
    int length;
  } size;
};

/* -------------------------------------------------------------------------- */
/* Example E */

struct shapeE {
  enum shape_tag tag;
  union {
    float radius;
    int length;
  };
};
