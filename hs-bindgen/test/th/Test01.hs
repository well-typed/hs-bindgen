-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test01 where

import HsBindgen.TH
import HsBindgen.Runtime.SizedByteArray (SizedByteArray (..)) -- TODO: GHC issue, we need to import this

-- Used by generated code
import Foreign.C.Types

$(getPackageRoot >>= \dir -> genBindings' [dir </> "examples"] "test_01.h")
