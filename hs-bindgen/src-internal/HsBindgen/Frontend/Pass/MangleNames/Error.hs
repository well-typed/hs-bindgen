module HsBindgen.Frontend.Pass.MangleNames.Error (
    MangleNamesError (..)
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

data MangleNamesError =
    MangleNamesCollision                    Hs.SomeName [C.WithLocationInfo C.DeclId]
  | MangleNamesCouldNotMangle               Text
  | MangleNamesCouldNotMangleSpecifiedName  Text
  | MangleNamesUnderlyingDeclNotMangled     C.DeclId (NonEmpty Hs.Namespace)
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace MangleNamesError where
  prettyForTrace = \case
      MangleNamesCollision x xs ->
        let intro = PP.hcat [
                "Colliding definitions for Haskell name "
              , prettyForTrace x
              , ":"
              ]
        in  PP.hang intro 2 $ PP.vcat $ map prettyForTrace xs
      MangleNamesCouldNotMangle name -> PP.hsep [
          "Could not mangle C name:"
        , PP.text name
        ]
      MangleNamesCouldNotMangleSpecifiedName name -> PP.hsep [
          "Could not mangle specified name:"
        , PP.text name
        ]
      MangleNamesUnderlyingDeclNotMangled declId namespaces -> PP.hsep [
          "Underlying"
        , PP.hsep $ List.intersperse "or" $
            map prettyForTrace $ NonEmpty.toList namespaces
        , "not mangled:"
        , prettyForTrace declId
        ]
