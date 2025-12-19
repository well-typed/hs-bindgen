{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Translation of Prim instances from Hs AST to SHs AST
module HsBindgen.Backend.SHs.Translation.Prim (
    translatePrimInstance
  ) where

import Data.Vec.Lazy (Vec (..))
import DeBruijn.Add (Add (..))
import DeBruijn.Idx

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.NameHint

import DeBruijn (S, Weaken (..), Wk (..), wk1)

{-------------------------------------------------------------------------------
  Main translation entry point
-------------------------------------------------------------------------------}

-- | Translate a Prim instance from Hs AST to SHs AST
translatePrimInstance ::
     Hs.Struct n
  -> Hs.PrimInstance
  -> Maybe HsDoc.Comment
  -> Instance
translatePrimInstance struct Hs.PrimInstance{..} mbComment =
    let indexBA   = lambda (lambda (translateDirectApply translateIndexByteArrayField)) primIndexByteArray
        readBA    = lambda (lambda (lambda (translateReadByteArrayFields struct))) primReadByteArray
        writeBA   = lambda (lambda (lambda (lambda (translateElimStruct translateWriteByteArrayFields)))) primWriteByteArray
        indexAddr = lambda (lambda (translateDirectApply translateIndexOffAddrField)) primIndexOffAddr
        readAddr  = lambda (lambda (lambda (translateReadOffAddrFields struct))) primReadOffAddr
        writeAddr = lambda (lambda (lambda (lambda (translateElimStruct translateWriteOffAddrFields)))) primWriteOffAddr
    in Instance
      { instanceClass = Prim_class
      , instanceArgs  = [TCon $ Hs.structName struct]
      , instanceSuperClasses = []
      , instanceTypes = []
      , instanceDecs  = [
            (Prim_sizeOf#         , EUnusedLam $ EUnboxedIntegral (toInteger primSizeOf))
          , (Prim_alignment#      , EUnusedLam $ EUnboxedIntegral (toInteger primAlignment))
          , (Prim_indexByteArray# , indexBA)
          , (Prim_readByteArray#  , readBA)
          , (Prim_writeByteArray# , writeBA)
          , (Prim_indexOffAddr#   , indexAddr)
          , (Prim_readOffAddr#    , readAddr)
          , (Prim_writeOffAddr#   , writeAddr)
          ]
      , instanceComment = mbComment
      }

{-------------------------------------------------------------------------------
  Index operations (pure reads)
-------------------------------------------------------------------------------}

-- | Translate DirectApply - direct constructor application (no applicative)
-- Generates: Constructor (field1) (field2) ...
translateDirectApply :: (f ctx -> SExpr ctx) -> Hs.Apply Hs.StructCon f ctx -> SExpr ctx
translateDirectApply translateField (Hs.Apply (Hs.StructCon struct) fields) =
    appManyExpr (ECon $ Hs.structConstr struct) (map translateField fields)

-- | Translate IndexByteArrayField to SExpr
--
-- Generates: @indexByteArray# arr (numFields *# i +# fieldPos)@
translateIndexByteArrayField :: Hs.IndexByteArrayField ctx -> SExpr ctx
translateIndexByteArrayField (Hs.IndexByteArrayField Hs.IndexPrimFieldData {..}) =
    appMany Prim_indexByteArray#
      [ EBound indexFieldArg1
      , computeIndex indexFieldArg2
                     indexFieldPos
                     indexNumFields
      ]

-- | Translate IndexOffAddrField to SExpr
--
-- Generates: @indexOffAddr# addr (numFields *# i +# fieldPos)@
translateIndexOffAddrField :: Hs.IndexOffAddrField ctx -> SExpr ctx
translateIndexOffAddrField (Hs.IndexOffAddrField Hs.IndexPrimFieldData {..}) =
    appMany Prim_indexOffAddr#
      [ EBound indexFieldArg1
      , computeIndex indexFieldArg2
                     indexFieldPos
                     indexNumFields
      ]

{-------------------------------------------------------------------------------
  Read operations (stateful reads)
-------------------------------------------------------------------------------}

-- | Translate ReadByteArrayFields with state threading
--
-- Generates nested case expressions with unboxed tuples:
-- > case readByteArray# arr (n *# i +# 0) s0 of
-- >   (# s1, x #) -> case readByteArray# arr (n *# i +# 1) s1 of
-- >     (# s2, y #) -> (# s2, Struct x y #)
translateReadByteArrayFields :: Hs.Struct n -> Hs.ReadByteArrayFields ctx -> SExpr ctx
translateReadByteArrayFields struct (Hs.ReadByteArrayFields Hs.ReadPrimFieldsData {..}) =
  buildNestedReads
    struct
    Prim_readByteArray#
    readFieldsArg1
    readFieldsArg2
    readFieldsArg3
    readFields
    []
    readNumFields

-- | Translate ReadOffAddrFields with state threading
--
-- Generates nested case expressions with unboxed tuples:
-- > case readOffAddr# addr (n *# i +# 0) s0 of
-- >   (# s1, x #) -> case readOffAddr# addr (n *# i +# 1) s1 of
-- >     (# s2, y #) -> (# s2, Struct x y #)
translateReadOffAddrFields :: Hs.Struct n -> Hs.ReadOffAddrFields ctx -> SExpr ctx
translateReadOffAddrFields struct (Hs.ReadOffAddrFields Hs.ReadPrimFieldsData {..}) =
  buildNestedReads
    struct
    Prim_readOffAddr#
    readFieldsArg1
    readFieldsArg2
    readFieldsArg3
    readFields
    []
    readNumFields

-- | Build nested case expressions for reading multiple fields with state threading
--
-- Threads state through each read operation, accumulating field values,
-- and constructs final unboxed tuple (# state, struct #) at the end.
--
-- For a struct with fields at positions [0, 1, 2], generates:
-- > case readOp arr (3 *# i +# 0) s0 of
-- >   (# s1, v0 #) -> case readOp arr (3 *# i +# 1) s1 of
-- >     (# s2, v1 #) -> case readOp arr (3 *# i +# 2) s2 of
-- >       (# s3, v2 #) -> (# s3, Struct v0 v1 v2 #)
--
-- For an empty struct (no fields), generates:
-- > (# state, EmptyStruct #)
buildNestedReads :: forall n ctx. Hs.Struct n -> Global -> Idx ctx -> Idx ctx -> Idx ctx -> [(HsType, Int)] -> [Idx ctx] -> Int -> SExpr ctx
buildNestedReads struct _ _ _ stateIdx [] _ _ =
  -- Empty struct: just return (# state, EmptyStruct #)
  -- No memory reads needed since there's no data to read
  EUnboxedTup
    [ EBound stateIdx
    , ECon (Hs.structConstr struct)  -- Empty constructor
    ]
buildNestedReads struct readOp arrIdx elemIdx stateIdx [(_, fieldPos)] valueVariables numFields =
  -- Last field: read it and construct the final unboxed tuple (# state, Struct ... #)
  let readCall = mkPrimRead arrIdx elemIdx stateIdx fieldPos
  in ECase readCall
      [ mkUnboxedTupleAlt $
          mkUnboxedTuple
            [ EBound stateIdxInTuple  -- Final state
            , mkStructValue (map EBound (weakenAllValues valueVariables) ++ [EBound valueIdxInTuple])
            ]
      ]
  where
    stateIdxInTuple = IZ    -- State is at IZ after matching (# s, v #)
    valueIdxInTuple = IS IZ -- Value is at IS IZ after matching (# s, v #)

    -- Construct a primitive read call: readOp arr (n *# i +# fieldPos) state
    mkPrimRead :: Idx ctx -> Idx ctx -> Idx ctx -> Int -> SExpr ctx
    mkPrimRead aIdx eIdx sIdx fPos =
      appMany readOp [ EBound aIdx
                     , computeIndexAt eIdx fPos numFields
                     , EBound sIdx
                     ]

    -- Construct an unboxed tuple expression: (# e1, e2 #)
    mkUnboxedTuple :: [SExpr (S (S ctx))] -> SExpr (S (S ctx))
    mkUnboxedTuple exprs = EUnboxedTup exprs

    -- Construct struct value by applying constructor to field expressions
    mkStructValue :: [SExpr (S (S ctx))] -> SExpr (S (S ctx))
    mkStructValue fieldExprs = ECon (Hs.structConstr struct) `appManyExpr` fieldExprs

    -- Construct pattern match alternative for unboxed tuple (# state, value #)
    mkUnboxedTupleAlt :: SExpr (S (S ctx)) -> SAlt ctx
    mkUnboxedTupleAlt body =
      SAltUnboxedTuple
        (AS (AS AZ))
        (    NameHint "s"
         ::: NameHint "v"
         ::: VNil
        )
        body

    -- Weaken all accumulated values when entering nested context
    weakenAllValues :: [Idx ctx] -> [Idx (S (S ctx))]
    weakenAllValues = map (weaken wk2)

buildNestedReads struct readOp arrIdx elemIdx stateIdx ((_, fieldPos):remainingFields) valueVariables numFields =
  -- Intermediate field: read it, add to accumulator, and recurse
  let -- After matching (# s, v #), we're in context (S (S ctx))
      -- All variables from ctx need weakening by 2 levels
      recursiveRead :: SExpr (S (S ctx))
      recursiveRead = buildNestedReads
          struct
          readOp
          (weaken wk2 arrIdx)  -- Weaken array index
          (weaken wk2 elemIdx) -- Weaken element index
          IZ                   -- New state from this read
          remainingFields      -- Remaining fields to read
          -- Accumulated values + current value
          (weakenAllValues valueVariables ++ [IS IZ])
          numFields

  in ECase (mkPrimRead arrIdx elemIdx stateIdx fieldPos)
      [ mkUnboxedTupleAlt recursiveRead ]
  where
    -- Construct a primitive read call: readOp arr (n *# i +# fieldPos) state
    mkPrimRead :: Idx ctx -> Idx ctx -> Idx ctx -> Int -> SExpr ctx
    mkPrimRead aIdx eIdx sIdx fPos =
      appMany readOp [ EBound aIdx
                     , computeIndexAt eIdx fPos numFields
                     , EBound sIdx
                     ]

    -- Construct pattern match alternative for unboxed tuple (# state, value #)
    mkUnboxedTupleAlt :: SExpr (S (S ctx)) -> SAlt ctx
    mkUnboxedTupleAlt body =
      SAltUnboxedTuple
        (AS (AS AZ))
        (    NameHint "s"
         ::: NameHint "v"
         ::: VNil
        )
        body

    -- Weaken all accumulated values when entering nested context
    weakenAllValues :: [Idx ctx] -> [Idx (S (S ctx))]
    weakenAllValues = map (weaken wk2)

{-------------------------------------------------------------------------------
  Write operations (stateful writes)
-------------------------------------------------------------------------------}

-- | Translate WriteByteArrayFields with state threading
--
-- Generates sequential writes with state threading:
-- > case writeByteArray# arr (n *# i +# 0) v0 s0 of
-- >   s1 -> case writeByteArray# arr (n *# i +# 1) v1 s1 of
-- >     s2 -> s2
--
-- writeByteArray# returns State# s, which we pattern match to thread through
translateWriteByteArrayFields :: Hs.WriteByteArrayFields ctx -> SExpr ctx
translateWriteByteArrayFields (Hs.WriteByteArrayFields Hs.WritePrimFieldsData {..}) =
  buildSequentialWrites
    Prim_writeByteArray#
    writeFieldsArg1
    writeFieldsArg2
    writeFieldsArg3
    writeFields
    writeNumFields

-- | Translate WriteOffAddrFields with state threading
--
-- Generates sequential writes with state threading:
-- > case writeOffAddr# addr (n *# i +# 0) v0 s0 of
-- >   s1 -> case writeOffAddr# addr (n *# i +# 1) v1 s1 of
-- >     s2 -> s2
--
-- writeOffAddr# returns State# s, which we pattern match to thread through
translateWriteOffAddrFields :: Hs.WriteOffAddrFields ctx -> SExpr ctx
translateWriteOffAddrFields (Hs.WriteOffAddrFields Hs.WritePrimFieldsData {..}) =
  buildSequentialWrites
    Prim_writeOffAddr#
    writeFieldsArg1
    writeFieldsArg2
    writeFieldsArg3
    writeFields
    writeNumFields

-- | Build sequential writes with state threading
--
-- Each write returns a new State# s, which is threaded to the next write.
-- Final result is the final State# s after all writes.
--
-- For a struct with fields at positions [0, 1], generates:
-- > case writeOp arr (2 *# i +# 0) v0 s0 of
-- >   s1 -> writeOp arr (2 *# i +# 1) v1 s1
--
-- For an empty struct (no fields), generates:
-- > state
buildSequentialWrites :: forall ctx. Global -> Idx ctx -> Idx ctx -> Idx ctx -> [(HsType, Int, Idx ctx)] -> Int -> SExpr ctx
buildSequentialWrites _ _ _ stateIdx [] _ =
  -- Empty struct: just return the state unchanged
  -- No memory writes needed since there's no data to write
  EBound stateIdx
buildSequentialWrites writeOp arrIdx elemIdx stateIdx [(_, fieldPos, fieldVal)] numFields =
  -- Last field: just perform the write and return the final state
  mkPrimWrite arrIdx elemIdx fieldVal stateIdx fieldPos
  where
    -- Construct a primitive write call: writeOp arr (n *# i +# fieldPos) val state
    mkPrimWrite :: Idx ctx -> Idx ctx -> Idx ctx -> Idx ctx -> Int -> SExpr ctx
    mkPrimWrite aIdx eIdx fVal sIdx fPos =
      appMany writeOp [EBound aIdx, computeIndexAt eIdx fPos numFields, EBound fVal, EBound sIdx]

buildSequentialWrites writeOp arrIdx elemIdx stateIdx ((_, fieldPos, fieldVal):remainingFields) numFields =
  -- Intermediate field: write it, pattern match on returned state, continue with remaining fields
  let -- After matching State#, we're in context (S ctx)
      -- All variables from ctx need weakening by 1 level
      remainingWrites :: SExpr (S ctx)
      remainingWrites = buildSequentialWrites
          writeOp
          (weaken wk1 arrIdx)
          (weaken wk1 elemIdx)
          IZ  -- New state from this write
          (weakenFieldValues remainingFields)
          numFields

  in ECase (mkPrimWrite arrIdx elemIdx fieldVal stateIdx fieldPos)
      [ mkStateAlt remainingWrites ]
  where
    -- Construct a primitive write call: writeOp arr (n *# i +# fieldPos) val state
    mkPrimWrite :: Idx ctx -> Idx ctx -> Idx ctx -> Idx ctx -> Int -> SExpr ctx
    mkPrimWrite aIdx eIdx fVal sIdx fPos =
      appMany writeOp [ EBound aIdx
                      , computeIndexAt eIdx fPos numFields
                      , EBound fVal
                      , EBound sIdx
                      ]

    -- Construct pattern match alternative for State# s
    -- Using empty name for irrefutable variable pattern
    mkStateAlt :: SExpr (S ctx) -> SAlt ctx
    mkStateAlt body =
      SAltNoConstr
        (NameHint "s" ::: VNil)
        body

    -- Weaken the value indices in remaining fields when entering nested context
    weakenFieldValues :: [(HsType, Int, Idx ctx)] -> [(HsType, Int, Idx (S ctx))]
    weakenFieldValues = map (\(ty, pos, val) -> (ty, pos, weaken wk1 val))

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Weakening witness for adding 2 levels (used when matching unboxed tuples)
wk2 :: Wk ctx (S (S ctx))
wk2 = SkipWk (SkipWk IdWk)

-- | Compute index arithmetic at a specific De Bruijn index
--
-- Generates: @numFields *# elemIdx +# fieldPos@
computeIndexAt :: Idx ctx -> Int -> Int -> SExpr ctx
computeIndexAt elemIdx fieldPos numFields =
    if numFields == 1
       then EBound elemIdx
       else appMany Prim_add#
              [ appMany Prim_mul# [numFieldsExpr, EBound elemIdx]
              , fieldPosExpr
              ]
  where
    numFieldsExpr = EUnboxedIntegral (toInteger numFields)
    fieldPosExpr  = EUnboxedIntegral (toInteger fieldPos)

-- | Compute array/addr index: numFields *# elementIndex +# fieldPosition
computeIndex :: Idx ctx -> Int -> Int -> SExpr ctx
computeIndex elemIdx fieldPos numFields =
    -- Generate: numFields# *# i# +# fieldPos#
    let numFieldsExpr = EUnboxedIntegral (toInteger numFields)
        fieldPosExpr  = EUnboxedIntegral (toInteger fieldPos)
        elemIdxExpr   = EBound elemIdx
    in if numFields == 1
       then elemIdxExpr  -- Optimization: if single field, just use element index
       else appMany Prim_add# [appMany Prim_mul# [numFieldsExpr, elemIdxExpr], fieldPosExpr]
