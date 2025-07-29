-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.TH.Test01 where

 -- TODO: GHC issue, we need to import this
import HsBindgen.Runtime.SizedByteArray (SizedByteArray (..))
import HsBindgen.TH

-- Used by generated code
import Foreign.C.Types

withHsBindgen def { extraQuoteIncludeDirs = [ RelativeToPkgRoot "examples" ] } $
  hashInclude $ CHeaderQuoteIncludePath "test_01.h"
