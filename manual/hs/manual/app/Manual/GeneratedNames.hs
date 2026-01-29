{-# LANGUAGE CPP #-}

module Manual.GeneratedNames (examples) where

import Example.Unsafe
import Manual.Tools

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Awkward names"

-- There's a quirk with Apple assembler and LLVM IR that do not accept
-- Unicode characters. So make sure to set SUPPORTS_UNICODE environment
-- variable only if you know your system supports it.
#if defined(SUPPORTS_UNICODE)
    -- On supporting platforms, call the functions with Unicode names.
    拜拜
    cϒ
#else
    -- On macOS/LLVM (e.g.), call the safe functions defined in your bindings module.
    -- We assume they are named `gamma` and `byeBye` in Haskell.
    byeBye
    gamma
#endif

    import'
