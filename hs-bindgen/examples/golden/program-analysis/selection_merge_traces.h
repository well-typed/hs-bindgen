struct X {
  int x;
};

struct Y {
  int y;
};

/* If we do not select X and Y, we want to get a _single_ trace: Could not
   select `f` because ... (in contrast to getting two traces) */
void dependsOnXAndY(struct X sx, struct Y sy);
