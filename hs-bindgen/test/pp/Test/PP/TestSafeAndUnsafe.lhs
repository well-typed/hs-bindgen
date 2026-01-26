[ "-I", "examples"
, "--module=Test.PP.TestSafeAndUnsafe"
, "--unique-id", "com.well-typed.hs-bindgen"
, "--single-file"
, "--safe", "_safe"
, "--unsafe", "_unsafe"
, "--log-as-info", "select-mangle-names-squashed"
, "test_01.h"
]
