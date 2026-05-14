/* The C identifiers `y` and `Y` lead to a single colliding Haskell identifier
   `Y`. */
union y;
union Y {
  int m;
  int o;
};
