#ifndef LIBRARY_CALLBACK_LOOP_A_H
#define LIBRARY_CALLBACK_LOOP_A_H

struct b;
struct a { int x; };
void a_visit(struct a *a, void (*cb)(struct a *, struct b *));

#endif
