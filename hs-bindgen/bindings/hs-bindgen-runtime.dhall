-- hs-bindgen binding specifications for hs-bindgen-runtime

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkTM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(headers : List Text) ->
      \(instances : List Text) ->
        { headers = map Text Text systemHeader headers
        , cname
        , haskell =
            { module = "HsBindgen.Runtime.LibC"
            , identifier
            }
        , instances
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
    : List ./HsBindgen/TypeMapping.dhall
        -- Floating Types
    = [ mkTM "fenv_t"       "CFenvT"    [ "fenv.h" ]              noI
      , mkTM "fexcept_t"    "CFexceptT" [ "fenv.h" ]              noI
        -- Mathematical Types
      , mkTM "div_t"        "CDivT"     [ "stdlib.h" ]            divI
      , mkTM "ldiv_t"       "CLdivT"    [ "stdlib.h" ]            divI
      , mkTM "lldiv_t"      "CLldivT"   [ "stdlib.h" ]            divI
      , mkTM "imaxdiv_t"    "CImaxdivT" [ "inttypes.h" ]          divI
        -- Wide Character Types
      , mkTM "wint_t"       "CWintT"    [ "wchar.h", "wctype.h" ] intI
      , mkTM "mbstate_t"    "CMbstateT" [ "uchar.h", "wchar.h" ]  noI
      , mkTM "wctrans_t"    "CWctransT" [ "wctype.h" ]            eqI
      , mkTM "wctype_t"     "CWctypeT"  [ "wchar.h", "wctype.h" ] eqI
      , mkTM "char16_t"     "CChar16T"  [ "uchar.h" ]             intI
      , mkTM "char32_t"     "CChar32T"  [ "uchar.h" ]             intI
        -- Time Types
      , mkTM "struct tm"    "CTm"       [ "time.h" ]              eqI
      ]

in  { types }
