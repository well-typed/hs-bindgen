{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Types.Structs (examples) where

import Control.Exception (bracket)
import Data.Vector.Storable qualified as VS
import Foreign as F
import Foreign.C (castCCharToChar)
import Foreign.C qualified as FC
import System.IO.Unsafe

import HsBindgen.Runtime.FLAM qualified as FLAM
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Manual.Tools
import Structs
import Structs.Safe

{-------------------------------------------------------------------------------
  Simple struct
-------------------------------------------------------------------------------}

mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c = unsafePerformIO $
    alloca $ \ptr -> do
      mk_triple (fromIntegral a) (fromIntegral b) (fromIntegral c) ptr
      peek ptr

{-------------------------------------------------------------------------------
  Structs with flexible array members
-------------------------------------------------------------------------------}

instance FLAM.NumElems FC.CChar Surname_Aux where
  numElems x = fromIntegral (surname_len x)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Structs"

    subsection "Simple struct"
    let triple = mkTriple 1 2 3
    print triple

    subsection "Flexible array members"
    let arr = IA.fromList $ fmap FC.castCharToCChar "Rich"
    bracket (IsA.withElemPtr arr $ \ptr -> surname_alloc (PtrConst.unsafeFromPtr ptr)) surname_free $
      \ptr -> do
        surname <- readRaw ptr
        putStrLn $ "The length of the surname is: " <> show (FLAM.numElems surname.aux)
        print $ VS.map castCCharToChar $ FLAM.flam surname
