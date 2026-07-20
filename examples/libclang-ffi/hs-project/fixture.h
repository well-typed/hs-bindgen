/* A small, fixed C input so the AST walk is short and deterministic. No
 * #includes, so the only cursors are the four declarations below (and their
 * fields / parameters). */

struct Point {
  int x;
  int y;
};

typedef struct Point Point;

int add(int a, int b);

extern int global_counter;
