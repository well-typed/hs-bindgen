-- hs-bindgen external bindings configuration for hs-bindgen-runtime

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(headers : List Text) ->
      \(instances : List Text) ->
        { cname
        , headers = map Text Text systemHeader headers
        , identifier
        , instances
        , module = "HsBindgen.Runtime.LibC"
        }

let noI  = [] : List Text
let eqI  = [ "Eq", "ReadRaw", "Show", "StaticSize", "Storable", "WriteRaw" ]
let divI = [ "Eq", "Ord", "ReadRaw", "Show" ]
let intI =
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

let types
    : List ./Mapping.dhall
        -- Floating Types
    = [ mkM "fenv_t"       "CFenvT"    [ "fenv.h" ]              noI
      , mkM "fexcept_t"    "CFexceptT" [ "fenv.h" ]              noI
        -- Mathematical Types
      , mkM "div_t"        "CDivT"     [ "stdlib.h" ]            divI
      , mkM "ldiv_t"       "CLdivT"    [ "stdlib.h" ]            divI
      , mkM "lldiv_t"      "CLldivT"   [ "stdlib.h" ]            divI
      , mkM "imaxdiv_t"    "CImaxdivT" [ "inttypes.h" ]          divI
        -- Wide Character Types
      , mkM "wint_t"       "CWintT"    [ "wchar.h", "wctype.h" ] intI
      , mkM "mbstate_t"    "CMbstateT" [ "uchar.h", "wchar.h" ]  noI
      , mkM "wctrans_t"    "CWctransT" [ "wctype.h" ]            eqI
      , mkM "wctype_t"     "CWctypeT"  [ "wchar.h", "wctype.h" ] eqI
      , mkM "char16_t"     "CChar16T"  [ "uchar.h" ]             intI
      , mkM "char32_t"     "CChar32T"  [ "uchar.h" ]             intI
        -- Time Types
      , mkM "struct tm"    "CTm"       [ "time.h" ]              eqI
      ]

in  { types }
