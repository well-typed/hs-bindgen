-- hs-bindgen shared library configuration for hs-bindgen-runtime

let map = https://prelude.dhall-lang.org/List/map

let systemHeader : Text -> Text =
      \(header : Text) ->
        "system:" ++ header

let mkM =
      \(cname : Text) ->
      \(identifier : Text) ->
      \(headers : List Text) ->
        { cname
        , headers = map Text Text systemHeader headers
        , identifier
        , module = "HsBindgen.Runtime.LibC"
        , package = "hs-bindgen-runtime"
        }

let types
    : List ./Mapping.dhall
        -- Floating Types
    = [ mkM "fenv_t"       "CFenvT"    [ "fenv.h" ]
      , mkM "fexcept_t"    "CFexceptT" [ "fenv.h" ]
        -- Mathematical Types
      , mkM "div_t"        "CDivT"     [ "stdlib.h" ]
      , mkM "ldiv_t"       "CLdivT"    [ "stdlib.h" ]
      , mkM "lldiv_t"      "CLldivT"   [ "stdlib.h" ]
      , mkM "imaxdiv_t"    "CImaxdivT" [ "inttypes.h" ]
        -- Wide Character Types
      , mkM "wint_t"       "CWintT"    [ "wchar.h", "wctype.h" ]
      , mkM "mbstate_t"    "CMbstateT" [ "uchar.h", "wchar.h" ]
      , mkM "wctrans_t"    "CWctransT" [ "wctype.h" ]
      , mkM "wctype_t"     "CWctypeT"  [ "wchar.h", "wctype.h" ]
      , mkM "char16_t"     "CChar16T"  [ "uchar.h" ]
      , mkM "char32_t"     "CChar32T"  [ "uchar.h" ]
        -- Time Types
      , mkM "struct tm"    "CTm"       [ "time.h" ]
      ]

in  { types }
