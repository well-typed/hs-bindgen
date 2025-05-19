-- This type is for specifying external bindings.  Features for binding
-- configuration are not included.

let TypeMapping
    : Type
    = { headers : List Text
      , cname : Text
      , module : Text
      , identifier : Text
      , instances : List Text
      }

in  TypeMapping
