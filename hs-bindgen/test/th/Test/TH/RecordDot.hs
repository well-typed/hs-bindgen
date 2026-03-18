{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: This module tests if we can handle duplicate record fields with
-- @OmitFieldPrefixes@.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- NOTE: This module also tests that we correctly attach documentation to record
-- fields, avoiding errors of the form
--
-- @
-- ‘Test.TH.RecordDot.x’ is not in scope at a reify
-- @
--
-- Please ensure that the language extension "NoFieldSelectors" is enabled.
{-# LANGUAGE NoFieldSelectors #-}

-- {-# OPTIONS_GHC -ddump-splices -ddump-deriv -ddump-to-file #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.TH.RecordDot where

import Optics ((%), (&), (.~))

import HsBindgen.TH

let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]
      & #fieldNamingStrategy       .~ OmitFieldPrefixes
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "duplicate_record_field.h"
