-- | Translation of C structs to Haskell Prim instances
module HsBindgen.Backend.Hs.Translation.Prim (
    mkPrimInstance
  ) where

import Data.Set qualified as Set
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports

import DeBruijn (Idx (..), Weaken (..), pattern IS, pattern IZ)

-- | Generate Prim instance declarations for a struct
--
-- Returns empty list if Prim is not in the instance set.
mkPrimInstance :: forall n.
     SNatI n
  => Set Hs.TypeClass       -- ^ Available instances
  -> Hs.Struct n            -- ^ Haskell struct
  -> C.Struct Final         -- ^ C struct
  -> [Hs.Decl]
mkPrimInstance insts hsStruct struct
  | Hs.Prim `Set.notMember` insts = []
  | otherwise = singleton
              $ Hs.DeclDefineInstance Hs.DefineInstance{
                    comment     = Nothing
                  , instanceDecl =
                      Hs.InstancePrim
                          hsStruct
                          Hs.PrimInstance {
                            Hs.primSizeOf    = struct.sizeof
                          , Hs.primAlignment = struct.alignment
                          -- indexByteArray# :: ByteArray# -> Int# -> a
                          , Hs.primIndexByteArray = Hs.Lambda "arr"
                                                  $ Hs.Lambda "i"
                                                  $ Hs.Apply (Hs.StructCon hsStruct)
                                                  $ indexedByteArrayFields (IS IZ) IZ
                          -- readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
                          , Hs.primReadByteArray = Hs.Lambda "arr"
                                                 $ Hs.Lambda "i"
                                                 $ Hs.Lambda "s"
                                                 $ Hs.ReadByteArrayFields readFieldData
                          -- writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
                          , Hs.primWriteByteArray = Hs.Lambda "arr"
                                                  $ Hs.Lambda "i"
                                                  $ Hs.Lambda "struct"
                                                  $ Hs.Lambda "s"
                                                  $ Hs.makeElimStruct (IS IZ) hsStruct
                                                  $ \wk xs ->
                                                      Hs.WriteByteArrayFields
                                                        Hs.WritePrimFieldsData
                                                           { Hs.writeFields = zipWith (\(ty, pos) x -> (ty, pos, x))
                                                                                      fieldTypesWithPos
                                                                                      (toList xs)
                                                           , Hs.writeFieldsArg1 = weaken wk (IS (IS (IS IZ)))
                                                           , Hs.writeFieldsArg2 = weaken wk (IS (IS IZ))
                                                           , Hs.writeFieldsArg3 = weaken wk IZ
                                                           , Hs.writeNumFields  = numFields
                                                           }
                          -- indexOffAddr# :: Addr# -> Int# -> a
                          , Hs.primIndexOffAddr = Hs.Lambda "addr"
                                                $ Hs.Lambda "i"
                                                $ Hs.Apply (Hs.StructCon hsStruct)
                                                $ indexedOffAddrFields (IS IZ) IZ
                          -- readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
                          , Hs.primReadOffAddr = Hs.Lambda "addr"
                                               $ Hs.Lambda "i"
                                               $ Hs.Lambda "s"
                                               $ Hs.ReadOffAddrFields readFieldData
                          -- writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
                          , Hs.primWriteOffAddr = Hs.Lambda "addr"
                                                $ Hs.Lambda "i"
                                                $ Hs.Lambda "struct"
                                                $ Hs.Lambda "s"
                                                $ Hs.makeElimStruct (IS IZ) hsStruct
                                                $ \wk xs ->
                                                    Hs.WriteOffAddrFields
                                                       { Hs.writeAddrFieldData = Hs.WritePrimFieldsData
                                                            { Hs.writeFields = zipWith (\(ty, pos) x -> (ty, pos, x))
                                                                                       fieldTypesWithPos
                                                                                       (toList xs)
                                                            , Hs.writeFieldsArg1 = weaken wk (IS (IS (IS IZ)))
                                                            , Hs.writeFieldsArg2 = weaken wk (IS (IS IZ))
                                                            , Hs.writeFieldsArg3 = weaken wk IZ
                                                            , Hs.writeNumFields  = numFields
                                                            }
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
        Hs.readFields = fieldTypesWithPos
      , Hs.readFieldsArg1 = IS (IS IZ)
      , Hs.readFieldsArg2 = IS IZ
      , Hs.readFieldsArg3 = IZ
      , Hs.readNumFields = numFields
      }

    -- | Generate IndexByteArrayField for each struct field
    indexedByteArrayFields :: Idx ctx -> Idx ctx -> [Hs.IndexByteArrayField ctx]
    indexedByteArrayFields arrIdx elemIdx =
      [ Hs.IndexByteArrayField
          Hs.IndexPrimFieldData
             { Hs.indexFieldType = fieldTy
             , Hs.indexFieldArg1 = arrIdx
             , Hs.indexFieldArg2 = elemIdx
             , Hs.indexFieldPos  = pos
             , Hs.indexNumFields = numFields
             }
      | (fieldTy, pos) <- fieldTypesWithPos
      ]

    -- | Generate IndexOffAddrField for each struct field
    indexedOffAddrFields :: Idx ctx -> Idx ctx -> [Hs.IndexOffAddrField ctx]
    indexedOffAddrFields addrIdx elemIdx =
      [ Hs.IndexOffAddrField
           Hs.IndexPrimFieldData
             { Hs.indexFieldType = fieldTy
             , Hs.indexFieldArg1 = addrIdx
             , Hs.indexFieldArg2 = elemIdx
             , Hs.indexFieldPos  = pos
             , Hs.indexNumFields = numFields
             }
      | (fieldTy, pos) <- fieldTypesWithPos
      ]
