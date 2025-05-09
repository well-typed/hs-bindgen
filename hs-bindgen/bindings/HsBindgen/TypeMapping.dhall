-- These types do not include features for binding configuration.

let HsRef
    : Type
    = { module : Text
      , identifier : Text
      }

let TypeMapping
    : Type
    = { headers : List Text
      , cname : Text
      , haskell : HsRef
      , instances : List Text
      }

in  TypeMapping
