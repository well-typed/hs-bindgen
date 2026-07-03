
/** @file
 * Block-typedef cases for typedef name-clash analysis
 *
 * Kept separate from typedef_analysis.h because this header requires -fblocks.
 *
 * The two cases mirror examples 13 and 14 from typedef_analysis.h: a tagged
 * type reached through a block type (rather than a function-pointer type) and
 * sharing its name with the surrounding typedef should be suffixed.
 */

// Block returning the eponymous struct: 'struct blk' is suffixed.
struct blk { int x; };
typedef struct blk (^blk)(void);

// Block taking the eponymous struct as an argument: 'struct blkarg' suffixed.
struct blkarg { int y; };
typedef int (^blkarg)(struct blkarg arg);
