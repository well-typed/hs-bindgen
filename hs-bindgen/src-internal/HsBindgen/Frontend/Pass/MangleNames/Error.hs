module HsBindgen.Frontend.Pass.MangleNames.Error (
    MangleNamesError (..)
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (SingleLoc)

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

data MangleNamesError =
    MangleNamesCollision                    Hs.SomeName [C.WithLocationInfo C.DeclId]
  | MangleNamesCouldNotMangle               Text
  | MangleNamesUnderlyingDeclNotMangled     C.DeclId (NonEmpty Hs.Namespace)
    -- | Two or more fields in the same struct mangle to the same Haskell name.
    --
    -- Only fires under 'OmitFieldPrefixes'; under 'AddFieldPrefixes' mangling
    -- is injective within a record.
  | MangleNamesDuplicateFieldName           Hs.SomeName [SingleLoc]
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
      MangleNamesUnderlyingDeclNotMangled declId namespaces -> PP.hsep [
          "Underlying"
        , PP.hsep $ List.intersperse "or" $
            map prettyForTrace $ NonEmpty.toList namespaces
        , "not mangled:"
        , prettyForTrace declId
        ]
      MangleNamesDuplicateFieldName x locs ->
        let intro = PP.hcat [
                "Duplicate record field name "
              , prettyForTrace x
              , ":"
              ]
        in  PP.hang intro 2 $ PP.vcat $ map PP.show locs
