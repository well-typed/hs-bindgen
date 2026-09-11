/*
 * Regression test for #2236: external binding spec resolution with
 * different #include spellings.
 *
 * This header reaches core.h via a relative path ("../core.h"), but the
 * external binding spec (consumer.yaml) references it as
 * "binding-specs/relative_include/core.h".  Before the RealPath switch
 * these two spellings would not match, so widget_t's external binding
 * would fail to resolve and widget_legacy_t would lose its external
 * representation.
 *
 * The generated Haskell code imports module M (from the binding spec),
 * which does not exist in this test fixture, since the test only checks that
 * the golden output matches, not that it compiles.
 */

#include "../core.h"

typedef struct widget_t widget_legacy_t;

void use_widget(widget_legacy_t *w);
