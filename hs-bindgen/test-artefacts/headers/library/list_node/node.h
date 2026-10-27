#ifndef LIBRARY_LIST_NODE_NODE_H
#define LIBRARY_LIST_NODE_NODE_H

#include "list.h"
struct node { struct list *owner; struct node *next; };

#endif
