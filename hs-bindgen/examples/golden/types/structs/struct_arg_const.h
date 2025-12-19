// See https://github.com/well-typed/hs-bindgen/issues/1439

struct thing {
    int x;
};

void               fun_const_arg   (const struct thing x);
const struct thing fun_const_result(void);
const struct thing fun_const       (const struct thing x);
