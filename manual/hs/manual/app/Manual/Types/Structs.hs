{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Types.Structs (examples) where

import Control.Exception (bracket)
import Data.Primitive.PrimArray qualified as PA
import Data.Vector.Storable qualified as VS
import Foreign as F
import Foreign.C (castCCharToChar)
import Foreign.C qualified as FC
import System.IO.Unsafe

import HsBindgen.Runtime.ConstPtr
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import HsBindgen.Runtime.IncompleteArray qualified as IA

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
    let arr = IA.fromList $ fmap FC.castCharToCChar "Rich"
    bracket (IA.withPtr arr $ \ptr -> surname_alloc (ConstPtr ptr)) surname_free $
      \ptr -> do
        (surname :: Structs.Surname) <- peek ptr
        putStrLn $ "The length of the surname is: " <> show (surname_len surname)
        (surnameWithFlam :: FLAM.WithFlexibleArrayMember FC.CChar Structs.Surname) <-
          FLAM.peekWithFLAM ptr
        let name :: VS.Vector FC.CChar
            name = FLAM.flamExtra surnameWithFlam
        print $ VS.map castCCharToChar name

    subsection "PrimArray of structs"
    -- Create a PrimArray of Triple structs
    let triples = PA.primArrayFromList
          [ mkTriple 1 2 3
          , mkTriple 4 5 6
          , mkTriple 7 8 9
          ]
    putStrLn $ "Created PrimArray with " <> show (PA.sizeofPrimArray triples) <> " elements"
    putStrLn $ "First element: " <> show (PA.indexPrimArray triples 0)
    putStrLn $ "Second element: " <> show (PA.indexPrimArray triples 1)
    putStrLn $ "Third element: " <> show (PA.indexPrimArray triples 2)
