// https://github.com/well-typed/hs-bindgen/issues/1415

enum test {
  test_a = 0,
  test_count
};

extern const char test_array[test_count];
