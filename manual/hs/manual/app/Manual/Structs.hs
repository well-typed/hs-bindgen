{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Structs (examples) where

import Control.Exception (bracket)
import Data.Vector.Storable qualified as VS
import Foreign as F
import Foreign.C (castCCharToChar)
import Foreign.C qualified as FC
import System.IO.Unsafe

import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import HsBindgen.Runtime.IncompleteArray qualified as IA

import Manual.Tools

import Example
import Example.Unsafe
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

instance FLAM.HasFlexibleArrayLength FC.CChar Surname where
  flexibleArrayMemberLength x = fromIntegral (surname_len x)

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
    bracket (surname_alloc $ IA.fromList $ fmap FC.castCharToCChar "Rich") surname_free $
      \ptr -> do
        (surname :: Structs.Surname) <- peek ptr
        putStrLn $ "The length of the surname is: " <> show (surname_len surname)
        (surnameWithFlam :: FLAM.WithFlexibleArrayMember FC.CChar Structs.Surname) <-
          FLAM.peekWithFLAM ptr
        let name :: VS.Vector FC.CChar
            name = FLAM.flamExtra surnameWithFlam
        print $ VS.map castCCharToChar name
