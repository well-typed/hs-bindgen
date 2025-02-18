{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.Pretty.Orphans where

-- base
import Data.Array.Byte
import Data.Foldable
  ( toList )
import Data.List.NonEmpty qualified as NE
import GHC.Exts qualified as IsList ( IsList(..) )

-- containers
import Data.IntMap.Strict
  ( IntMap )
import Data.IntMap.Strict qualified as IntMap
  ( assocs )

-- pretty-show
import Text.Show.Pretty
  ( PrettyVal(..) )
import Text.Show.Pretty qualified as Pretty

-- vec
import Data.Vec.Lazy
  ( Vec(..) )

-- c-expr
import C.Char qualified as C

--------------------------------------------------------------------------------

deriving anyclass instance PrettyVal a => PrettyVal ( NE.NonEmpty a )

instance PrettyVal a => PrettyVal ( Vec n a ) where
  prettyVal v = Pretty.Con "Vec" [ prettyVal ( toList v ) ]

instance PrettyVal a => PrettyVal ( IntMap a ) where
  prettyVal v = Pretty.Con "IntMap" [ prettyVal ( IntMap.assocs v ) ]

instance PrettyVal ByteArray where
  prettyVal ba = Pretty.Con "ByteArray" [ prettyVal ( IsList.toList ba ) ]

instance PrettyVal C.CharValue
