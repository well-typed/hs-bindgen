#ifndef LIBRARY_WINDOW_WIDGET_WINDOW_H
#define LIBRARY_WINDOW_WIDGET_WINDOW_H

struct widget;
struct window { int width; };
void window_add(struct window *w, struct widget *child);

#endif
