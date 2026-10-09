// window_widget/window.h: a window holds widgets.  A function here takes the
// struct of widget.h.
#ifndef WINDOW_WIDGET_WINDOW_H
#define WINDOW_WIDGET_WINDOW_H

struct widget;

struct window {
  int width;
};

void window_add(struct window *w, struct widget *child);

#endif
