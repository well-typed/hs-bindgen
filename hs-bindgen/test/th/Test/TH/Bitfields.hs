-- | Bit-field tests
--
-- Note that this generates bindings for a header that is also tested using
-- golden tests.  This module is used by "Test.Bitfields" to implement property
-- tests.

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

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.TH.Bitfields where

import Optics ((%), (&), (.~))

import HsBindgen.TH

let cfg :: Config
    cfg = def
      -- Include directory @examples/golden@ is /not/ used because include
      -- directory @examples@ is used in the Cabal configuration.
      & #clang % #extraIncludeDirs .~ [Pkg "examples"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "golden/types/structs/bitfields.h"
