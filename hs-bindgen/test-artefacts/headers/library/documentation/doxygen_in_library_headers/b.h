#ifndef DOXYGEN_IN_LIBRARY_HEADERS_B_H
#define DOXYGEN_IN_LIBRARY_HEADERS_B_H

/** A list of nodes */
struct b_list {
  int length; /**< Number of nodes in the list */
};

/** The list that new nodes join */
extern struct b_list b_default;

#endif
