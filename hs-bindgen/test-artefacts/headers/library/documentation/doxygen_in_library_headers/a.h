#ifndef DOXYGEN_IN_LIBRARY_HEADERS_A_H
#define DOXYGEN_IN_LIBRARY_HEADERS_A_H

#include "b.h"

/**
 * A node of a list.
 *
 * @see b_list
 */
struct a_node {
  /** The stored value */
  int value;
  /** The list that owns this node */
  struct b_list *owner;
};

/** Number of nodes, at most */
#define A_MAX 16

/**
 * Detach a node from its list.
 *
 * @param node The node to detach
 * @return 0 on success
 */
int a_detach(struct a_node *node);

#endif
