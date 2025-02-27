-- hs-bindgen shared library configuration for base

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(module : Text) ->
      \(headers : List Text) ->
        { cname
        , headers = map Text Text systemHeader headers
        , identifier
        , module
        , package = "base"
        }

let intTypesH = [ "inttypes.h", "stdint.h" ]

let types
    : List ./Mapping.dhall
        -- Integral Types
    = [ mkM "int8_t"         "Int8"     "Data.Int"        intTypesH
      , mkM "int16_t"        "Int16"    "Data.Int"        intTypesH
      , mkM "int32_t"        "Int32"    "Data.Int"        intTypesH
      , mkM "int64_t"        "Int64"    "Data.Int"        intTypesH
      , mkM "uint8_t"        "Word8"    "Data.Word"       intTypesH
      , mkM "uint16_t"       "Word16"   "Data.Word"       intTypesH
      , mkM "uint32_t"       "Word32"   "Data.Word"       intTypesH
      , mkM "uint64_t"       "Word64"   "Data.Word"       intTypesH
      , mkM "int_least8_t"   "Int8"     "Data.Int"        intTypesH
      , mkM "int_least16_t"  "Int16"    "Data.Int"        intTypesH
      , mkM "int_least32_t"  "Int32"    "Data.Int"        intTypesH
      , mkM "int_least64_t"  "Int64"    "Data.Int"        intTypesH
      , mkM "uint_least8_t"  "Word8"    "Data.Word"       intTypesH
      , mkM "uint_least16_t" "Word16"   "Data.Word"       intTypesH
      , mkM "uint_least32_t" "Word32"   "Data.Word"       intTypesH
      , mkM "uint_least64_t" "Word64"   "Data.Word"       intTypesH
      , mkM "int_fast8_t"    "Int8"     "Data.Int"        intTypesH
      , mkM "int_fast16_t"   "Int16"    "Data.Int"        intTypesH
      , mkM "int_fast32_t"   "Int32"    "Data.Int"        intTypesH
      , mkM "int_fast64_t"   "Int64"    "Data.Int"        intTypesH
      , mkM "uint_fast8_t"   "Word8"    "Data.Word"       intTypesH
      , mkM "uint_fast16_t"  "Word16"   "Data.Word"       intTypesH
      , mkM "uint_fast32_t"  "Word32"   "Data.Word"       intTypesH
      , mkM "uint_fast64_t"  "Word64"   "Data.Word"       intTypesH
      , mkM "intmax_t"       "CIntMax"  "Foreign.C.Types" intTypesH
      , mkM "uintmax_t"      "CUIntMax" "Foreign.C.Types" intTypesH
      , mkM "intptr_t"       "CIntPtr"  "Foreign.C.Types" intTypesH
      , mkM "uintptr_t"      "CUIntPtr" "Foreign.C.Types" intTypesH
        -- Standard Definitions
      , mkM "size_t" "CSize" "Foreign.C.Types"
          [ "signal.h"
          , "stddef.h"
          , "stdio.h"
          , "stdlib.h"
          , "string.h"
          , "time.h"
          , "uchar.h"
          , "wchar.h"
          ]
      , mkM "ptrdiff_t" "CPtrdiff" "Foreign.C.Types" [ "stddef.h" ]
        -- Non-Local Jump Types
      , mkM "jmp_buf" "CJmpBuf" "Foreign.C.Types" [ "setjmp.h" ]
        -- Wide Character Types
      , mkM "wchar_t" "CWchar" "Foreign.C.Types"
          [ "inttypes.h", "stddef.h", "stdlib.h", "wchar.h" ]
        -- Time Types
      , mkM "time_t"  "CTime"  "Foreign.C.Types" [ "signal.h", "time.h" ]
      , mkM "clock_t" "CClock" "Foreign.C.Types" [ "signal.h", "time.h" ]
        -- File Types
      , mkM "FILE"   "CFile" "Foreign.C.Types" [ "stdio.h", "wchar.h" ]
      , mkM "fpos_t" "CFpos" "Foreign.C.Types" [ "stdio.h" ]
        -- Signal Types
      , mkM "sig_atomic_t" "CSigAtomic" "Foreign.C.Types" [ "signal.h" ]
      ]

in  { types }
