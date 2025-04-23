-- hs-bindgen external bindings configuration for base

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(module : Text) ->
      \(headers : List Text) ->
      \(instances : List Text) ->
        { cname
        , headers = map Text Text systemHeader headers
        , identifier
        , instances
        , module
        }

let intTypesH = [ "inttypes.h", "stdint.h" ]

let noI   = [] : List Text
let intI  =
      [ "Bits"
      , "Bounded"
      , "Enum"
      , "Eq"
      , "FiniteBits"
      , "Integral"
      , "Ix"
      , "Num"
      , "Ord"
      , "Read"
      , "ReadRaw"
      , "Real"
      , "Show"
      , "StaticSize"
      , "Storable"
      , "WriteRaw"
      ]
let timeI =
      [ "Enum"
      , "Eq"
      , "Num"
      , "Ord"
      , "Read"
      , "ReadRaw"
      , "Real"
      , "Show"
      , "StaticSize"
      , "Storable"
      , "WriteRaw"
      ]

let types
    : List ./Mapping.dhall
        -- Integral Types
    = [ mkM "int8_t"         "Int8"     "Data.Int"        intTypesH intI
      , mkM "int16_t"        "Int16"    "Data.Int"        intTypesH intI
      , mkM "int32_t"        "Int32"    "Data.Int"        intTypesH intI
      , mkM "int64_t"        "Int64"    "Data.Int"        intTypesH intI
      , mkM "uint8_t"        "Word8"    "Data.Word"       intTypesH intI
      , mkM "uint16_t"       "Word16"   "Data.Word"       intTypesH intI
      , mkM "uint32_t"       "Word32"   "Data.Word"       intTypesH intI
      , mkM "uint64_t"       "Word64"   "Data.Word"       intTypesH intI
      , mkM "int_least8_t"   "Int8"     "Data.Int"        intTypesH intI
      , mkM "int_least16_t"  "Int16"    "Data.Int"        intTypesH intI
      , mkM "int_least32_t"  "Int32"    "Data.Int"        intTypesH intI
      , mkM "int_least64_t"  "Int64"    "Data.Int"        intTypesH intI
      , mkM "uint_least8_t"  "Word8"    "Data.Word"       intTypesH intI
      , mkM "uint_least16_t" "Word16"   "Data.Word"       intTypesH intI
      , mkM "uint_least32_t" "Word32"   "Data.Word"       intTypesH intI
      , mkM "uint_least64_t" "Word64"   "Data.Word"       intTypesH intI
      , mkM "int_fast8_t"    "Int8"     "Data.Int"        intTypesH intI
      , mkM "int_fast16_t"   "Int16"    "Data.Int"        intTypesH intI
      , mkM "int_fast32_t"   "Int32"    "Data.Int"        intTypesH intI
      , mkM "int_fast64_t"   "Int64"    "Data.Int"        intTypesH intI
      , mkM "uint_fast8_t"   "Word8"    "Data.Word"       intTypesH intI
      , mkM "uint_fast16_t"  "Word16"   "Data.Word"       intTypesH intI
      , mkM "uint_fast32_t"  "Word32"   "Data.Word"       intTypesH intI
      , mkM "uint_fast64_t"  "Word64"   "Data.Word"       intTypesH intI
      , mkM "intmax_t"       "CIntMax"  "Foreign.C.Types" intTypesH intI
      , mkM "uintmax_t"      "CUIntMax" "Foreign.C.Types" intTypesH intI
      , mkM "intptr_t"       "CIntPtr"  "Foreign.C.Types" intTypesH intI
      , mkM "uintptr_t"      "CUIntPtr" "Foreign.C.Types" intTypesH intI
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
          intI
      , mkM "ptrdiff_t" "CPtrdiff" "Foreign.C.Types" [ "stddef.h" ] intI
        -- Non-Local Jump Types
      , mkM "jmp_buf" "CJmpBuf" "Foreign.C.Types" [ "setjmp.h" ] noI
        -- Wide Character Types
      , mkM "wchar_t" "CWchar" "Foreign.C.Types"
          [ "inttypes.h", "stddef.h", "stdlib.h", "wchar.h" ] intI
        -- Time Types
      , mkM "time_t"  "CTime"  "Foreign.C.Types" [ "signal.h", "time.h" ] timeI
      , mkM "clock_t" "CClock" "Foreign.C.Types" [ "signal.h", "time.h" ] timeI
        -- File Types
      , mkM "FILE"   "CFile" "Foreign.C.Types" [ "stdio.h", "wchar.h" ] noI
      , mkM "fpos_t" "CFpos" "Foreign.C.Types" [ "stdio.h" ]            noI
        -- Signal Types
      , mkM "sig_atomic_t" "CSigAtomic" "Foreign.C.Types" [ "signal.h" ] intI
      ]

in  { types }
