-- | Forget 'Ann'otations and coerce the remainder
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.ForgetAnn (
    ForgetAnn (forgetAnn)
  , coerceNonMacroType
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

type In  = PrepareReparse

-- | Forget 'Ann'otations and coerce the remainder
--
-- When a declaration is not reparsed (because it does not contain any macro
-- expanions), we simply want to coerce the declaration to the next pass
-- parameter. Before we can coerce, we also have to forget 'Ann'otations.
class ForgetAnn a where
  forgetAnn :: a In -> a ReparseMacroExpansions

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance ForgetAnn C.StructField where
  forgetAnn field = C.StructField{
          typ    = coercePass field.typ
        , ann    = NoAnn
        , offset = field.offset
        , width  = field.width
        , info   = coercePass field.info
        }

instance ForgetAnn C.UnionField where
  forgetAnn field = C.UnionField{
          typ    = coercePass field.typ
        , ann    = NoAnn
        , info   = coercePass field.info
        }

instance ForgetAnn C.Typedef where
  forgetAnn typedef = C.Typedef{
          typ = coercePass typedef.typ
        , ann = NoAnn
        }

instance ForgetAnn C.Function where
  forgetAnn fun = C.Function{
          args  = fmap coercePass fun.args
        , res   = coercePass fun.res
        , attrs = fun.attrs
        , ann   = NoAnn
        }

instance ForgetAnn C.Global where
  forgetAnn global = C.Global {
        typ = coercePass global.typ
      , ann = NoAnn
      }

{-------------------------------------------------------------------------------
  Coerce
-------------------------------------------------------------------------------}

-- | Coerce a 'C.Type' from 'PrepareReparse' to 'ReparseMacroExpansions' (no
-- 'TypeMacro' present).
coerceNonMacroType ::
     C.Type PrepareReparse
  -> C.Type ReparseMacroExpansions
coerceNonMacroType = coercePass
