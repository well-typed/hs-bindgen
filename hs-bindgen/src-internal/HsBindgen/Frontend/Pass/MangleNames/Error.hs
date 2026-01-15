module HsBindgen.Frontend.Pass.MangleNames.Error (
    MangleNamesFailure (..)
  ) where
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

data MangleNamesFailure =
    MangleNamesCollision Hs.Identifier [WithLocationInfo DeclId]
  | MangleNamesCouldNotMangle Text
  deriving stock (Show)

instance PrettyForTrace MangleNamesFailure where
  prettyForTrace = \case
      MangleNamesCollision x xs ->
        let intro = PP.hcat [
                "Colliding definitions for Haskell identifier "
              , PP.text x.text
              , ":"
              ]
        in  PP.hang intro 2 $ PP.vcat $ map prettyForTrace xs
      MangleNamesCouldNotMangle name -> PP.hsep [
          "Could not mangle C name:"
        , PP.text name
        ]
