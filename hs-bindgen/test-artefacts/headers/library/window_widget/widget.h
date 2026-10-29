#ifndef LIBRARY_WINDOW_WIDGET_WIDGET_H
#define LIBRARY_WINDOW_WIDGET_WIDGET_H

struct window;
struct widget { int id; };
struct window *widget_window(struct widget *w);

#endif
