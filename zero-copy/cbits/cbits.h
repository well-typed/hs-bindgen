#pragma once

struct point {
    int x;
    int y;
};

struct rect {
    struct point topleft;
    struct point bottomright;
};

void show_rect(struct rect* r);

union pointVsArray {
    struct point asPoint;
    int asArray[2];
};