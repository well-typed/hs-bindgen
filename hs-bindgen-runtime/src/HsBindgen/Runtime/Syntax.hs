{-# LANGUAGE TypeFamilies #-}

module HsBindgen.Runtime.Syntax
  ( IntLike, FloatLike
  )
  where

-- base
import Prelude hiding
  ( Eq(..), Ord(..), Num(..), Fractional(..) )
import Prelude qualified

--------------------------------------------------------------------------------

type IntLike   a = Prelude.Integral   a => a
type FloatLike a = Prelude.Fractional a => a
