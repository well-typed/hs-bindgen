{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-deriv #-}

module Test.TH.RecordDot where

import Optics ((%), (&), (.~))

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]
      -- TODO-D.
      -- & #fieldNamingStrategy       .~ EnableRecordDot
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "duplicate_record_dot.h"

-- [d| data A = A { a :: Int }
--     data B = B { a :: Int } |]

-- x = A 10
-- y = x.a
