/** @file
 * Test case for Javadoc banner-style comments (issue #1846)
 */

/******************************************************************************
 * A function documented with a Javadoc banner-style comment.
 *
 * The decorative asterisks should be stripped by doxygen, producing clean
 * documentation without any '*' noise.
 *
 * @param x The input value
 * @return The doubled value
 *****************************************************************************/
int banner_double(int x);

/******************************************************************************
 * A struct documented with a Javadoc banner comment.
 *****************************************************************************/
struct banner_point {
    /****
     * X coordinate
     ****/
    int x;

    /****
     * Y coordinate
     ****/
    int y;
};
