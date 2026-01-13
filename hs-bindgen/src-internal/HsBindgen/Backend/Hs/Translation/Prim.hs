-- | Translation of C structs to Haskell Prim instances
module HsBindgen.Backend.Hs.Translation.Prim (
    mkPrimInstance
  ) where

import Data.Set qualified as Set
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Idx (..), Weaken (..), pattern IS, pattern IZ)

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst

-- | Generate Prim instance declarations for a struct
--
-- Returns empty list if Prim is not in the instance set.
mkPrimInstance :: forall n.
     SNatI n
  => Set Inst.TypeClass  -- ^ Available instances
  -> Hs.Struct n         -- ^ Haskell struct
  -> C.Struct Final      -- ^ C struct
  -> [Hs.Decl]
mkPrimInstance insts hsStruct struct
  | Inst.Prim `Set.notMember` insts = []
  | otherwise = singleton $ Hs.DeclDefineInstance Hs.DefineInstance{
        comment     = Nothing
      , instanceDecl = Hs.InstancePrim hsStruct Hs.PrimInstance {
            sizeOf    = struct.sizeof
          , alignment = struct.alignment

            -- indexByteArray# :: ByteArray# -> Int# -> a
          , indexByteArray =
                Hs.Lambda "arr"
              $ Hs.Lambda "i"
              $ Hs.Apply (Hs.StructCon hsStruct)
              $ indexedByteArrayFields (IS IZ) IZ

            -- readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
          , readByteArray =
                Hs.Lambda "arr"
              $ Hs.Lambda "i"
              $ Hs.Lambda "s"
              $ Hs.ReadByteArrayFields readFieldData

            -- writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
          , writeByteArray =
                Hs.Lambda "arr"
              $ Hs.Lambda "i"
              $ Hs.Lambda "struct"
              $ Hs.Lambda "s"
              $ Hs.makeElimStruct (IS IZ) hsStruct
              $ \wk xs -> Hs.WriteByteArrayFields Hs.WritePrimFieldsData{
                    fields    = zipWith
                                  (\(ty, pos) x -> (ty, pos, x))
                                  fieldTypesWithPos
                                  (toList xs)
                  , arg1      = weaken wk (IS (IS (IS IZ)))
                  , arg2      = weaken wk (IS (IS IZ))
                  , arg3      = weaken wk IZ
                  , numFields = numFields
                  }

            -- indexOffAddr# :: Addr# -> Int# -> a
          , indexOffAddr =
                Hs.Lambda "addr"
              $ Hs.Lambda "i"
              $ Hs.Apply (Hs.StructCon hsStruct)
              $ indexedOffAddrFields (IS IZ) IZ

            -- readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
          , readOffAddr = Hs.Lambda "addr"
              $ Hs.Lambda "i"
              $ Hs.Lambda "s"
              $ Hs.ReadOffAddrFields readFieldData

            -- writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
          , writeOffAddr =
                Hs.Lambda "addr"
              $ Hs.Lambda "i"
              $ Hs.Lambda "struct"
              $ Hs.Lambda "s"
              $ Hs.makeElimStruct (IS IZ) hsStruct
              $ \wk xs -> Hs.WriteOffAddrFields Hs.WritePrimFieldsData{
                    fields    = zipWith
                                  (\(ty, pos) x -> (ty, pos, x))
                                  fieldTypesWithPos
                                  (toList xs)
                  , arg1      = weaken wk (IS (IS (IS IZ)))
                  , arg2      = weaken wk (IS (IS IZ))
                  , arg3      = weaken wk IZ
                  , numFields = numFields
                  }
          }
      }
  where
    numFields :: Int
    numFields = Vec.length hsStruct.fields

    fieldTypesWithPos :: [(HsType, Int)]
    fieldTypesWithPos = [(f.typ, pos) | (f, pos) <- zip (toList hsStruct.fields) [0..]]

    -- readFieldData :: Hs.ReadPrimFieldsData ctx
    readFieldData = Hs.ReadPrimFieldsData {
          fields    = fieldTypesWithPos
        , arg1      = IS (IS IZ)
        , arg2      = IS IZ
        , arg3      = IZ
        , numFields = numFields
        }

    -- | Generate IndexByteArrayField for each struct field
    indexedByteArrayFields :: Idx ctx -> Idx ctx -> [Hs.IndexByteArrayField ctx]
    indexedByteArrayFields arrIdx elemIdx =
      [ Hs.IndexByteArrayField Hs.IndexPrimFieldData{
            typ       = fieldTy
          , arg1      = arrIdx
          , arg2      = elemIdx
          , pos       = pos
          , numFields = numFields
          }
      | (fieldTy, pos) <- fieldTypesWithPos
      ]

    -- | Generate IndexOffAddrField for each struct field
    indexedOffAddrFields :: Idx ctx -> Idx ctx -> [Hs.IndexOffAddrField ctx]
    indexedOffAddrFields addrIdx elemIdx =
      [ Hs.IndexOffAddrField Hs.IndexPrimFieldData{
            typ       = fieldTy
          , arg1      = addrIdx
          , arg2      = elemIdx
          , pos       = pos
          , numFields = numFields
          }
      | (fieldTy, pos) <- fieldTypesWithPos
      ]
