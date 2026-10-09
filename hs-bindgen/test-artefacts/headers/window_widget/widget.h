// window_widget/widget.h: a widget knows its window.  A function here returns
// the struct of window.h.
#ifndef WINDOW_WIDGET_WIDGET_H
#define WINDOW_WIDGET_WIDGET_H

struct window;

struct widget {
  int id;
};

struct window *widget_window(struct widget *w);

#endif
