-- | Shared helpers for the c-expr-dsl test suite.
module Test.CExpr.Util (
    fakeLoc
  ) where

import Clang.HighLevel.Types

-- | A synthetic source location used to satisfy constructors that carry a
-- 'MultiLoc' (notably 'C.Expr.Syntax.Macro' and 'Token') in tests where the
-- actual location is irrelevant.
fakeLoc :: MultiLoc
fakeLoc = MultiLoc{
      multiLocExpansion = SingleLoc{
          singleLocPath   = "<test>"
        , singleLocLine   = 1
        , singleLocColumn = 1
        , singleLocOffset = 1
        }
    , multiLocPresumed  = Nothing
    , multiLocSpelling  = Nothing
    , multiLocFile      = Nothing
    }
