-- hs-bindgen binding specifications for base

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkTM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(module : Text) ->
      \(headers : List Text) ->
      \(instances : List Text) ->
        { headers = map Text Text systemHeader headers
        , cname
        , module
        , identifier
        , instances
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
    : List ./HsBindgen/TypeMapping.dhall
        -- Integral Types
    = [ mkTM "int8_t"         "Int8"     "Data.Int"        intTypesH intI
      , mkTM "int16_t"        "Int16"    "Data.Int"        intTypesH intI
      , mkTM "int32_t"        "Int32"    "Data.Int"        intTypesH intI
      , mkTM "int64_t"        "Int64"    "Data.Int"        intTypesH intI
      , mkTM "uint8_t"        "Word8"    "Data.Word"       intTypesH intI
      , mkTM "uint16_t"       "Word16"   "Data.Word"       intTypesH intI
      , mkTM "uint32_t"       "Word32"   "Data.Word"       intTypesH intI
      , mkTM "uint64_t"       "Word64"   "Data.Word"       intTypesH intI
      , mkTM "int_least8_t"   "Int8"     "Data.Int"        intTypesH intI
      , mkTM "int_least16_t"  "Int16"    "Data.Int"        intTypesH intI
      , mkTM "int_least32_t"  "Int32"    "Data.Int"        intTypesH intI
      , mkTM "int_least64_t"  "Int64"    "Data.Int"        intTypesH intI
      , mkTM "uint_least8_t"  "Word8"    "Data.Word"       intTypesH intI
      , mkTM "uint_least16_t" "Word16"   "Data.Word"       intTypesH intI
      , mkTM "uint_least32_t" "Word32"   "Data.Word"       intTypesH intI
      , mkTM "uint_least64_t" "Word64"   "Data.Word"       intTypesH intI
      , mkTM "int_fast8_t"    "Int8"     "Data.Int"        intTypesH intI
      , mkTM "int_fast16_t"   "Int16"    "Data.Int"        intTypesH intI
      , mkTM "int_fast32_t"   "Int32"    "Data.Int"        intTypesH intI
      , mkTM "int_fast64_t"   "Int64"    "Data.Int"        intTypesH intI
      , mkTM "uint_fast8_t"   "Word8"    "Data.Word"       intTypesH intI
      , mkTM "uint_fast16_t"  "Word16"   "Data.Word"       intTypesH intI
      , mkTM "uint_fast32_t"  "Word32"   "Data.Word"       intTypesH intI
      , mkTM "uint_fast64_t"  "Word64"   "Data.Word"       intTypesH intI
      , mkTM "intmax_t"       "CIntMax"  "Foreign.C.Types" intTypesH intI
      , mkTM "uintmax_t"      "CUIntMax" "Foreign.C.Types" intTypesH intI
      , mkTM "intptr_t"       "CIntPtr"  "Foreign.C.Types" intTypesH intI
      , mkTM "uintptr_t"      "CUIntPtr" "Foreign.C.Types" intTypesH intI
        -- Standard Definitions
      , mkTM "size_t" "CSize" "Foreign.C.Types"
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
      , mkTM "ptrdiff_t" "CPtrdiff" "Foreign.C.Types" [ "stddef.h" ] intI
        -- Non-Local Jump Types
      , mkTM "jmp_buf" "CJmpBuf" "Foreign.C.Types" [ "setjmp.h" ] noI
        -- Wide Character Types
      , mkTM "wchar_t" "CWchar" "Foreign.C.Types"
          [ "inttypes.h", "stddef.h", "stdlib.h", "wchar.h" ] intI
        -- Time Types
      , mkTM "time_t"  "CTime"  "Foreign.C.Types" [ "signal.h", "time.h" ] timeI
      , mkTM "clock_t" "CClock" "Foreign.C.Types" [ "signal.h", "time.h" ] timeI
        -- File Types
      , mkTM "FILE"   "CFile" "Foreign.C.Types" [ "stdio.h", "wchar.h" ] noI
      , mkTM "fpos_t" "CFpos" "Foreign.C.Types" [ "stdio.h" ]            noI
        -- Signal Types
      , mkTM "sig_atomic_t" "CSigAtomic" "Foreign.C.Types" [ "signal.h" ] intI
      ]

in  { types }
