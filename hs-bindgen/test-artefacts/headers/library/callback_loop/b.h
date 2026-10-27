#ifndef LIBRARY_CALLBACK_LOOP_B_H
#define LIBRARY_CALLBACK_LOOP_B_H

struct a;
struct b { int y; };
void b_visit(struct b *b, void (*cb)(struct b *, struct a *));

#endif
