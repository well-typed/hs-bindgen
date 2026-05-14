struct a;

struct b {
  struct a *toA;
};

struct a {
  struct b toB;
};
