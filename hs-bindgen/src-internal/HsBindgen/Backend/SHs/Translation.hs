{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Simplified HS translation (from high level HS)
module HsBindgen.Backend.SHs.Translation (
    translateDecls,
    translateType,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec
import DeBruijn.Add (Add (..))
import DeBruijn.Idx

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Macro
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import DeBruijn (Weaken (..), Wk (..), wk1)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls ::
  ByCategory_ [Hs.Decl] -> ByCategory_ ([CWrapper], [SDecl])
translateDecls = fmap go
  where
    go :: [Hs.Decl] -> ([CWrapper], [SDecl])
    go decls = (wrappers, concatMap translateDecl decls)
      where
        wrappers = getCWrappers decls

-- Find and assemble C sources required by foreign imports.
getCWrappers :: [Hs.Decl] -> [CWrapper]
getCWrappers decls = mapMaybe getCWrapper decls
  where
    getCWrapper :: Hs.Decl -> Maybe CWrapper
    getCWrapper = \case
      Hs.DeclForeignImport (Hs.ForeignImportDecl{foreignImportCallConv}) ->
        case foreignImportCallConv of
          CallConvUserlandCAPI w -> Just w
          _otherCallConv         -> Nothing
      _otherDecl -> Nothing

translateDecl :: Hs.Decl -> [SDecl]
translateDecl (Hs.DeclData d)           = singleton $ translateDeclData d
translateDecl (Hs.DeclEmpty d)          = singleton $ translateDeclEmpty d
translateDecl (Hs.DeclNewtype n)        = singleton $ translateNewtype n
translateDecl (Hs.DeclDefineInstance i) = singleton $ translateDefineInstanceDecl i
translateDecl (Hs.DeclDeriveInstance i) = singleton $ translateDeriveInstance i
translateDecl (Hs.DeclMacroExpr e)      = singleton $ translateMacroExpr e
translateDecl (Hs.DeclForeignImport i)  = translateForeignImportDecl i
translateDecl (Hs.DeclFunction f)       = singleton $ translateFunctionDecl f
translateDecl (Hs.DeclPatSyn ps)        = singleton $ translatePatSyn ps
translateDecl (Hs.DeclUnionGetter u)    = singleton $ translateUnionGetter u
translateDecl (Hs.DeclUnionSetter u)    = singleton $ translateUnionSetter u
translateDecl (Hs.DeclVar d)            = [DVar d]
translateDecl (Hs.DeclPragma d)         = [DPragma d]

translateDefineInstanceDecl :: Hs.DefineInstance -> SDecl
translateDefineInstanceDecl Hs.DefineInstance {..} =
  case defineInstanceDeclarations of
    Hs.InstanceStorable struct i -> DInst $ translateStorableInstance struct i defineInstanceComment
    Hs.InstancePrim struct i -> DInst $ translatePrimInstance struct i defineInstanceComment
    Hs.InstanceHasCField i -> DInst $ translateHasCFieldInstance i defineInstanceComment
    Hs.InstanceHasCBitfield i -> DInst $ translateHasCBitfieldInstance i defineInstanceComment
    Hs.InstanceHasField i -> DInst $ translateHasFieldInstance i defineInstanceComment
    Hs.InstanceHasFLAM struct fty i -> DInst
      Instance
        { instanceClass   = HasFlexibleArrayMember_class
        , instanceArgs    = [ translateType fty, TCon $ Hs.structName struct ]
        , instanceSuperClasses = []
        , instanceTypes   = []
        , instanceDecs    = [(HasFlexibleArrayMember_offset, ELam "_ty" $ EIntegral (toInteger i) Nothing)]
        , instanceComment = defineInstanceComment
        }
    Hs.InstanceCEnum struct fTyp vMap isSequential ->
      DInst $ translateCEnumInstance struct fTyp vMap isSequential defineInstanceComment
    Hs.InstanceSequentialCEnum struct nameMin nameMax ->
      DInst $ translateSequentialCEnum struct nameMin nameMax defineInstanceComment
    Hs.InstanceCEnumShow struct ->
      DInst $ translateCEnumInstanceShow struct defineInstanceComment
    Hs.InstanceCEnumRead struct ->
      DInst $ translateCEnumInstanceRead struct defineInstanceComment
    Hs.InstanceToFunPtr Hs.ToFunPtrInstance{..}     -> DInst
      Instance
        { instanceClass   = ToFunPtr_class
        , instanceArgs    = [ translateType toFunPtrInstanceType ]
        , instanceSuperClasses = []
        , instanceTypes   = []
        , instanceDecs    = [( ToFunPtr_toFunPtr
                             , EFree $ Hs.InternalName toFunPtrInstanceBody )]
        , instanceComment = defineInstanceComment
        }
    Hs.InstanceFromFunPtr Hs.FromFunPtrInstance{..} -> DInst
      Instance
        { instanceClass   = FromFunPtr_class
        , instanceArgs    = [ translateType fromFunPtrInstanceType ]
        , instanceSuperClasses = []
        , instanceTypes   = []
        , instanceDecs    = [( FromFunPtr_fromFunPtr
                             , EFree $ Hs.InternalName fromFunPtrInstanceBody )]
        , instanceComment = defineInstanceComment
        }

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord
  Record
    { dataType = Hs.structName struct
    , dataCon  = Hs.structConstr struct
    , dataFields =
        [ Field {
              fieldName    = Hs.fieldName f
            , fieldType    = translateType $ Hs.fieldType f
            , fieldOrigin  = Hs.fieldOrigin f
            , fieldComment = Hs.fieldComment f
            }
        | f <- toList $ Hs.structFields struct
        ]
    , dataOrigin =
        case Hs.structOrigin struct of
          Just origin -> origin
          Nothing     -> panicPure "Missing structOrigin"
    , dataDeriv   = []
    , dataComment = Hs.structComment struct
    }

translateDeclEmpty :: Hs.EmptyData -> SDecl
translateDeclEmpty d = DEmptyData
  EmptyData
    { emptyDataName    = Hs.emptyDataName d
    , emptyDataOrigin  = Hs.emptyDataOrigin d
    , emptyDataComment = Hs.emptyDataComment d
    }

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype
  Newtype
    { newtypeName   = Hs.newtypeName n
    , newtypeCon    = Hs.newtypeConstr n
    , newtypeField  = Field {
          fieldName    = Hs.fieldName $ Hs.newtypeField n
        , fieldType    = translateType . Hs.fieldType $ Hs.newtypeField n
        , fieldOrigin  = Hs.fieldOrigin $ Hs.newtypeField n
        , fieldComment = Hs.fieldComment $ Hs.newtypeField n
        }
    , newtypeOrigin  = Hs.newtypeOrigin n
    , newtypeDeriv   = []
    , newtypeComment = Hs.newtypeComment n
    }

translateDeriveInstance :: Hs.DeriveInstance -> SDecl
translateDeriveInstance Hs.DeriveInstance{..} = DDerivingInstance
  DerivingInstance {
        derivingInstanceStrategy = fmap translateType deriveInstanceStrategy
      , derivingInstanceType     = TApp (translateTypeClass deriveInstanceClass) (TCon deriveInstanceName)
      , derivingInstanceComment  = deriveInstanceComment
      }

translateTypeClass :: Hs.TypeClass -> ClosedType
translateTypeClass Hs.Bits               = TGlobal Bits_class
translateTypeClass Hs.Bounded            = TGlobal Bounded_class
translateTypeClass Hs.Enum               = TGlobal Enum_class
translateTypeClass Hs.Eq                 = TGlobal Eq_class
translateTypeClass Hs.FiniteBits         = TGlobal FiniteBits_class
translateTypeClass Hs.Floating           = TGlobal Floating_class
translateTypeClass Hs.Fractional         = TGlobal Fractional_class
translateTypeClass Hs.Integral           = TGlobal Integral_class
translateTypeClass Hs.Ix                 = TGlobal Ix_class
translateTypeClass Hs.Num                = TGlobal Num_class
translateTypeClass Hs.Ord                = TGlobal Ord_class
translateTypeClass Hs.Prim               = TGlobal Prim_class
translateTypeClass Hs.Read               = TGlobal Read_class
translateTypeClass Hs.ReadRaw            = TGlobal ReadRaw_class
translateTypeClass Hs.Real               = TGlobal Real_class
translateTypeClass Hs.RealFloat          = TGlobal RealFloat_class
translateTypeClass Hs.RealFrac           = TGlobal RealFrac_class
translateTypeClass Hs.Show               = TGlobal Show_class
translateTypeClass Hs.StaticSize         = TGlobal StaticSize_class
translateTypeClass Hs.Storable           = TGlobal Storable_class
translateTypeClass Hs.WriteRaw           = TGlobal WriteRaw_class
translateTypeClass Hs.HasBaseForeignType = TGlobal HasBaseForeignType_class

translateForeignImportDecl :: Hs.ForeignImportDecl -> [SDecl]
translateForeignImportDecl Hs.ForeignImportDecl { foreignImportParameters = args
                                                , foreignImportResultType = resType
                                                , ..
                                                } =
    [  DForeignImport ForeignImport
        { foreignImportParameters =
          map (\Hs.FunctionParameter { functionParameterType = argType
                                     , ..
                                     } ->
                    FunctionParameter
                      { functionParameterType = translateType argType
                      , ..
                      }
                ) args
        , foreignImportResultType = translateType resType
        , ..
        }
    ]

translateFunctionDecl :: Hs.FunctionDecl -> SDecl
translateFunctionDecl Hs.FunctionDecl {..} = DFunction
  Function { functionName       = functionDeclName
           , functionParameters = map translateFunctionParameter functionDeclParameters
           , functionResultType = translateType functionDeclResultType
           , functionBody       = functionDeclBody
           , functionComment    = functionDeclComment
           }
  where
    translateFunctionParameter :: Hs.FunctionParameter -> FunctionParameter
    translateFunctionParameter Hs.FunctionParameter{..} =
      FunctionParameter
        { functionParameterType = translateType functionParameterType
        , ..
        }


translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn Hs.PatSyn {..} = DPatternSynonym
  PatternSynonym
    { patSynName
    , patSynOrigin
    , patSynComment
    , patSynType = TCon patSynType
    , patSynRHS  = PEApps patSynConstr [PELit patSynValue]
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType = \case
    Hs.HsPrimType t         -> TGlobal (PrimType t)
    Hs.HsTypRef r           -> TCon r
    Hs.HsConstArray n t     -> TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t  -> TGlobal IncompleteArray `TApp` (translateType t)
    Hs.HsPtr t              -> TApp (TGlobal Foreign_Ptr) (translateType t)
    Hs.HsFunPtr t           -> TApp (TGlobal Foreign_FunPtr) (translateType t)
    Hs.HsStablePtr t        -> TApp (TGlobal Foreign_StablePtr) (translateType t)
    Hs.HsConstPtr t         -> TApp (TGlobal ConstPtr_type) (translateType t)
    Hs.HsIO t               -> TApp (TGlobal IO_type) (translateType t)
    Hs.HsFun a b            -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding r c hs  -> TExt r c hs
    Hs.HsByteArray          -> TGlobal ByteArray_type
    Hs.HsSizedByteArray n m -> TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t            -> TGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t      -> TApp (TGlobal ComplexType) (translateType (HsPrimType t))
    Hs.HsStrLit s           -> TStrLit s

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance ::
     Hs.Struct n
  -> Hs.StorableInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStorableInstance struct Hs.StorableInstance{..} mbComment = do
    let peek = lambda (idiom structCon translatePeekCField) storablePeek
    let poke = lambda (lambda (translateElimStruct (doAll translatePokeCField))) storablePoke
    Instance
      { instanceClass = Storable_class
      , instanceArgs  = [TCon $ Hs.structName struct]
      , instanceSuperClasses = []
      , instanceTypes = []
      , instanceDecs  = [
            (Storable_sizeOf    , EUnusedLam $ EInt storableSizeOf)
          , (Storable_alignment , EUnusedLam $ EInt storableAlignment)
          , (Storable_peek      , peek)
          , (Storable_poke      , poke)
          ]
      , instanceComment = mbComment
      }

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
            (Prim_sizeOf#         , EUnusedLam $ EIntegral (toInteger primSizeOf) (Just HsPrimUnboxedInt))
          , (Prim_alignment#      , EUnusedLam $ EIntegral (toInteger primAlignment) (Just HsPrimUnboxedInt))
          , (Prim_indexByteArray# , indexBA)
          , (Prim_readByteArray#  , readBA)
          , (Prim_writeByteArray# , writeBA)
          , (Prim_indexOffAddr#   , indexAddr)
          , (Prim_readOffAddr#    , readAddr)
          , (Prim_writeOffAddr#   , writeAddr)
          ]
      , instanceComment = mbComment
      }

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

-- | Weakening witness for adding 2 levels (used when matching unboxed tuples)
wk2 :: Wk ctx (S (S ctx))
wk2 = SkipWk (SkipWk IdWk)

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

-- | Compute index arithmetic at a specific De Bruijn index
--
computeIndexAt :: Idx ctx -> Int -> Int -> SExpr ctx
computeIndexAt elemIdx fieldPos numFields =
    if numFields == 1
       then EBound elemIdx
       else appMany Prim_add#
              [ appMany Prim_mul# [numFieldsExpr, EBound elemIdx]
              , fieldPosExpr
              ]
  where
    numFieldsExpr = EIntegral (toInteger numFields) (Just HsPrimUnboxedInt)
    fieldPosExpr  = EIntegral (toInteger fieldPos) (Just HsPrimUnboxedInt)

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

-- | Compute array/addr in *# elementIndex +# fieldPosition
computeIndex :: Idx ctx -> Int -> Int -> SExpr ctx
computeIndex elemIdx fieldPos numFields =
    -- Generate: numFields# *# i# +# fieldPos#
    let numFieldsExpr = EIntegral (toInteger numFields) (Just HsPrimUnboxedInt)
        fieldPosExpr  = EIntegral (toInteger fieldPos) (Just HsPrimUnboxedInt)
        elemIdxExpr   = EBound elemIdx
    in if numFields == 1
       then elemIdxExpr  -- Optimization: if single field, just use element index
       else appMany Prim_add# [appMany Prim_mul# [numFieldsExpr, elemIdxExpr], fieldPosExpr]


translatePeekCField :: Hs.PeekCField ctx -> SExpr ctx
translatePeekCField (Hs.PeekCField field ptr) = appMany HasCField_peekCField [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekCBitfield field ptr) = appMany HasCBitfield_peekCBitfield [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]

translatePokeCField :: Hs.PokeCField ctx -> SExpr ctx
translatePokeCField (Hs.PokeCField field ptr x) = appMany HasCField_pokeCField [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeCBitfield field ptr x) = appMany HasCBitfield_pokeCBitfield [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]

{-------------------------------------------------------------------------------
  'HasCField'
-------------------------------------------------------------------------------}

translateHasCFieldInstance ::
     Hs.HasCFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCFieldInstance Hs.HasCFieldInstance{..} mbComment = do
    Instance {
        instanceClass   = HasCField_class
      , instanceArgs    = [parentType, fieldNameLitType]
      , instanceSuperClasses = []
      , instanceTypes   = [
            (HasCField_CFieldType, [parentType, fieldNameLitType], fieldType)
          ]
      , instanceDecs    = [
            (HasCField_offset#, EUnusedLam $ EUnusedLam $ EIntegral o Nothing)
          ]
      , instanceComment = mbComment
      }
  where
    parentType = translateType hasCFieldInstanceParentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasCFieldInstanceFieldName
    fieldType = translateType hasCFieldInstanceCFieldType
    o = fromIntegral hasCFieldInstanceFieldOffset

{-------------------------------------------------------------------------------
  'HasCBitfield'
-------------------------------------------------------------------------------}

translateHasCBitfieldInstance ::
     Hs.HasCBitfieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCBitfieldInstance Hs.HasCBitfieldInstance{..} mbComment = do
    Instance
      { instanceClass   = HasCBitfield_class
      , instanceArgs    = [parentType, fieldNameLitType]
      , instanceSuperClasses = []
      , instanceTypes   = [
            (HasCBitfield_CBitfieldType, [parentType, fieldNameLitType], fieldType)
          ]
      , instanceDecs    = [
            (HasCBitfield_bitOffset#, EUnusedLam $ EUnusedLam $ EIntegral o Nothing )
          , (HasCBitfield_bitWidth#, EUnusedLam $ EUnusedLam $ EIntegral w Nothing)
          ]
      , instanceComment = mbComment
      }
  where
    parentType = translateType hasCBitfieldInstanceParentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasCBitfieldInstanceFieldName
    fieldType = translateType hasCBitfieldInstanceCBitfieldType
    o = fromIntegral hasCBitfieldInstanceBitOffset
    w = fromIntegral hasCBitfieldInstanceBitWidth

translateHasFieldInstance ::
     Hs.HasFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldInstance Hs.HasFieldInstance{..} mbComment = do
    Instance {
        instanceClass   = HasField_class
      , instanceArgs    = [fieldNameLitType, parentPtr, tyPtr]
      , instanceSuperClasses = [
            (NomEq_class, [
                tyTypeVar
              , TGlobal fieldTypeGlobal `TApp` parentType `TApp` fieldNameLitType]
              )
          ]
      , instanceTypes   = []
      , instanceDecs    = [
            (HasField_getField,
              EGlobal ptrToFieldGlobal `EApp`
              (EGlobal Proxy_constructor `ETypeApp` fieldNameLitType)
            )
          ]
      , instanceComment = mbComment
      }
  where
    (fieldTypeGlobal, ptrToFieldGlobal, tyPtr) = case hasFieldInstanceVia of
      Hs.ViaHasCField    ->
        ( HasCField_CFieldType
        , HasCField_ptrToCField
        , TGlobal Foreign_Ptr `TApp` tyTypeVar
        )
      Hs.ViaHasCBitfield ->
        ( HasCBitfield_CBitfieldType
        , HasCBitfield_ptrToCBitfield
        , TGlobal HasCBitfield_BitfieldPtr `TApp` parentType `TApp` fieldNameLitType
        )

    parentType = translateType hasFieldInstanceParentType
    parentPtr = TGlobal Foreign_Ptr `TApp` parentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasFieldInstanceFieldName
    -- TODO: this is not actually a free type variable. See issue #1287.
    tyTypeVar = TFree $ Hs.ExportedName "ty"

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct :: (forall ctx'. t ctx' -> SExpr ctx') -> Hs.ElimStruct t ctx -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x struct add k) = ECase
    (EBound x)
    [SAlt (Hs.structConstr struct) add hints (f k)]
  where
    hints = fmap (toNameHint . Hs.fieldName) $ Hs.structFields struct

toNameHint :: Hs.Name 'Hs.NsVar -> NameHint
toNameHint = NameHint . T.unpack . Hs.getName

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

translateUnionGetter :: Hs.UnionGetter -> SDecl
translateUnionGetter Hs.UnionGetter{..} = DFunction
  Function { functionName       = unionGetterName
           , functionParameters = [ FunctionParameter
                                     { functionParameterName    = Nothing
                                     , functionParameterType    = TCon unionGetterConstr
                                     , functionParameterComment = Nothing
                                     }
                                  ]
           , functionResultType = translateType unionGetterType
           , functionBody       = EGlobal ByteArray_getUnionPayload
           , functionComment    = unionGetterComment
           }

translateUnionSetter :: Hs.UnionSetter -> SDecl
translateUnionSetter Hs.UnionSetter{..} = DFunction
  Function { functionName       = unionSetterName
           , functionParameters = [ FunctionParameter
                                     { functionParameterName    = Nothing
                                     , functionParameterType    = translateType unionSetterType
                                     , functionParameterComment = Nothing
                                     }
                                  ]
           , functionResultType = TCon unionSetterConstr
           , functionBody       = EGlobal ByteArray_setUnionPayload
           , functionComment    = unionSetterComment
           }

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

translateCEnumInstance ::
     Hs.Struct (S Z)
  -> HsType
  -> Map Integer (NonEmpty String)
  -> Bool
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential mbComment = Instance {
      instanceClass = CEnum_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = [(CEnumZ_tycon, [tcon], translateType fTyp)]
    , instanceDecs  = [
          (CEnum_toCEnum, ECon (Hs.structConstr struct))
        , (CEnum_fromCEnum, EFree fname)
        , (CEnum_declaredValues, EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared, EApp (EGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared, EApp (EGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
        ] ++ seqDecs
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

    dconStrE :: SExpr ctx
    dconStrE = EString . T.unpack $ Hs.getName (Hs.structConstr struct)

    fname :: Hs.Name Hs.NsVar
    fname = Hs.fieldName $
      NonEmpty.head (Vec.toNonEmpty (Hs.structFields struct))

    declaredValuesE :: SExpr ctx
    declaredValuesE = EApp (EGlobal CEnum_declaredValuesFromList) $ EList [
        ETup [
            EIntegral v Nothing
          , if null names
              then EApp (EGlobal NonEmpty_singleton) (EString name)
              else
                EInfix
                  NonEmpty_constructor
                  (EString name)
                  (EList (EString <$> names))
          ]
      | (v, name :| names) <- Map.toList vMap
      ]

    seqDecs :: [(Global, ClosedExpr)]
    seqDecs
      | isSequential = [
            (CEnum_isDeclared, EGlobal CEnum_seqIsDeclared)
          , (CEnum_mkDeclared, EGlobal CEnum_seqMkDeclared)
          ]
      | otherwise = []

translateSequentialCEnum ::
     Hs.Struct (S Z)
  -> Hs.Name Hs.NsConstr
  -> Hs.Name Hs.NsConstr
  -> Maybe HsDoc.Comment
  -> Instance
translateSequentialCEnum struct nameMin nameMax mbComment = Instance {
      instanceClass = SequentialCEnum_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceShow ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceShow struct mbComment = Instance {
      instanceClass = Show_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (Show_showsPrec, EGlobal CEnum_showsCEnum)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      instanceClass = Read_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (Read_readPrec, EGlobal CEnum_readPrecCEnum)
        , (Read_readList, EGlobal Read_readListDefault)
        , (Read_readListPrec, EGlobal Read_readListPrecDefault)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

{-------------------------------------------------------------------------------
  Internal auxiliary: derived functionality
-------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr ctx] -> SExpr ctx
appMany = appManyExpr . EGlobal

appManyExpr :: SExpr ctx -> [SExpr ctx] -> SExpr ctx
appManyExpr = foldl' EApp

-- | Struct constructor
structCon :: Hs.StructCon ctx -> SExpr ctx
structCon (Hs.StructCon s) = ECon (Hs.structConstr s)

-- | Idiom brackets
idiom :: (pure ctx -> SExpr ctx) -> (xs ctx -> SExpr ctx) -> Hs.Ap pure xs ctx -> SExpr ctx
idiom f g (Hs.Ap p xs) = foldl'
    (\ acc x -> EInfix Applicative_seq acc (g x))
    (EApp (EGlobal Applicative_pure) (f p))
    xs

-- | Translate lambda
lambda :: (t (S ctx) -> SExpr (S ctx)) -> Hs.Lambda t ctx -> SExpr ctx
lambda f (Hs.Lambda hint t) = ELam hint (f t)

-- | Monad sequencing
doAll :: (t ctx -> SExpr ctx) -> Hs.Seq t ctx -> SExpr ctx
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal (Tuple_constructor 0)
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
