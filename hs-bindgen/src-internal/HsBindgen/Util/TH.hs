{-# LANGUAGE CPP #-}

module HsBindgen.Util.TH (
    getPackageRoot
  )
where

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif
