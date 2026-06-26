-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Backend.Hs.Translation (
    generateDeclarations
  ) where

import Control.Monad.Reader qualified as Reader
import Control.Monad.State qualified as State hiding (MonadState)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import DeBruijn (Add (..), Idx (..), pattern I2)

import HsBindgen.Backend.Category
import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.Function
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.Structure
import HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.Hs.Translation.Union
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.PrettyC qualified as PC
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type
import HsBindgen.NameHint

import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations ::
     HasMacroTypes l
  => MacroLang l
  -> UniqueId
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex l
  -> C.Sizeofs
  -> [C.Decl l Final]
  -> ByCategory_ [Hs.Decl l]
generateDeclarations macroLang uniqueId config name declIndex sizeofs =
    fmap reverse .
      foldl' partitionBindingCategories mempty .
      generateDeclarations' macroLang env declIndex
  where
    partitionBindingCategories ::
      ByCategory_ [a] -> WithCategory a  -> ByCategory_ [a]
    partitionBindingCategories xs (WithCategory cat decl) =
      over (lensForCategory cat) (decl :) xs

    supInsts :: Inst.SupportedInstances
    supInsts = def

    env :: HsM.Env
    env = HsM.initEnv uniqueId name config sizeofs supInsts

-- | Internal. Top-level declaration with foreign import category.
data WithCategory a = WithCategory {
    _withCategoryCategory :: Category
  , _withCategoryDecl     :: a
  } deriving (Show)

generateDeclarations' ::
     HasMacroTypes l
  => MacroLang l
  -> HsM.Env
  -> DeclIndex l
  -> [C.Decl l Final]
  -> [WithCategory (Hs.Decl l)]
generateDeclarations' macroLang env declIndex decs =
    HsM.runHsM env $ do
      let scannedFunctionTypes = scanAllFunctionTypes decs
          -- Generate ToFunPtr/FromFunPtr instances for nested function types
          -- These go in the main module to avoid orphan instances
          fFIStubsAndFunPtrInstances =
                   [ WithCategory CType d
                   | (args, res) <- Set.toList scannedFunctionTypes
                   , not (any C.hasUnsupportedType (res: fmap (.typ) args))
                   , any (isDefinedInCurrentModule declIndex) (res: fmap (.typ) args)
                   , d <- ToFromFunPtr.forFunction env.sizeofs (args, res)
                   ]
      hsDeclsAction <- mapM (generateDecs macroLang) decs
      hsDecls <- HsM.runAction $ fmap concat $ sequence hsDeclsAction
      pure $ hsDecls ++ fFIStubsAndFunPtrInstances

-- | This function takes a list of all declarations and collects all function
-- types (with some exceptions).
--
-- For example: function types that exist as arguments to other functions
-- (nth-order functions), fields of structs, fields of unions, etc.
--
-- This explicitly excludes typedefs that are function types (.e.g, @typedef
-- void F()@) or function type indirections (e.g., @typedef void (*F)()@)
-- because these are handled separately by 'typedefDecs'
-- and'typedefFunTypeIndirectionDecs' respectively.
--
scanAllFunctionTypes :: [C.Decl l Final] -> Set ([C.TypeFunArg Final], C.Type Final)
scanAllFunctionTypes = foldMap $ \decl ->
    case decl.kind of
      C.DeclStruct struct ->
        foldMap (C.getAllFunTypes . (.typ)) struct.fields
      C.DeclUnion union   ->
        foldMap (C.getAllFunTypes . (.typ)) union.fields
      C.DeclTypedef typedef
        -- Exclude function type indirections, because they are already handled
        -- by 'typedefFuntypeIndirectionDecs'
        | Just (args, res, _) <- C.getFirstFunTypeIndirection typedef.typ
        -> foldMap C.getAllFunTypes (res : map (.typ) args)
        -- Exclude /direct/ function types, because they are already handled by
        -- 'typedefDecs'
        | otherwise
        -> C.getAllFunTypeIndirections typedef.typ
      C.DeclEnum enum -> C.getAllFunTypes enum.typ
      C.DeclAnonEnumConstant{} -> Set.empty
      C.DeclOpaque{} -> Set.empty
      C.DeclMacro{} -> Set.empty
      C.DeclFunction fn ->
        foldMap C.getAllFunTypes (fn.res : map (.argTyp.typ) fn.args)
      C.DeclGlobal g -> C.getAllFunTypes g.typ

-- | Check if a type is defined in the current module
isDefinedInCurrentModule :: DeclIndex l -> C.Type Final -> Bool
isDefinedInCurrentModule declIndex =
    any (isInDeclIndex . snd) . C.depsOfType
  where
    isInDeclIndex :: DeclIdPair -> Bool
    isInDeclIndex declId = isJust $ DeclIndex.lookup declId.cName declIndex

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1758>
-- Take the 'PrescriptiveDeclSpec' into account.
generateDecs ::
     HasMacroTypes l
  => MacroLang l
  -> C.Decl l Final
  -> HsM (HsM.Action [WithCategory (Hs.Decl l)])
generateDecs macroLang (C.Decl info kind spec) =
    case kind of
      C.DeclStruct struct -> withCategoryM CType $
        HsM.immediateM $ structDecs info struct spec
      C.DeclUnion union -> withCategoryM CType $
        HsM.immediateM $ unionDecs info union spec
      C.DeclEnum enum -> withCategoryM CType $
        HsM.immediateM $ enumDecs info enum spec
      C.DeclAnonEnumConstant anonEnumConst -> withCategoryM CType $ do
        HsM.immediateM $ anonEnumConstantDecs info anonEnumConst
      C.DeclTypedef typedef -> withCategoryM CType $
        case C.getFirstFunTypeIndirection typedef.typ of
          Just (args, res, reconstruct) ->
            typedefFunTypeIndirectionDecs info (args, res, reconstruct) typedef.names spec
          Nothing ->
            HsM.immediateM $ typedefDecs info Origin.Typedef typedef spec
      C.DeclOpaque mSize -> withCategoryM CType $
        HsM.immediateM $ opaqueDecs info spec mSize
      C.DeclFunction function -> do
        let funDeclsWith safety =
              functionDecs safety info function spec
            funType = C.TypeFun (map (.argTyp) function.args) function.res
            -- Declare a function pointer. We can pass this 'FunPtr' to C
            -- functions that take a function pointer of the appropriate type.
            funPtrDecls = fst <$>
                addressStubDecs info funType HaskellId spec
        safes   <- withCategoryM (CTerm CSafe)   (HsM.immediateM $ funDeclsWith SHs.Safe)
        unsafes <- withCategoryM (CTerm CUnsafe) (HsM.immediateM $ funDeclsWith SHs.Unsafe)
        funPtrs <- withCategoryM (CTerm CFunPtr) (HsM.immediateM $ funPtrDecls)
        pure $ HsM.concatSequence [safes, unsafes, funPtrs]
      C.DeclMacro macro -> withCategoryM CType $
        HsM.immediateM $ macroDecs macroLang info macro spec
      C.DeclGlobal g -> do
        withCategoryM (CTerm CGlobal) $
          HsM.immediateM $ global info g.typ spec
  where
    withCategory ::
         Category
      -> HsM.Action [a]
      -> HsM.Action [WithCategory a]
    withCategory c = fmap (map (WithCategory c))

    withCategoryM ::
         Functor m
      => Category
      -> m (HsM.Action [a])
      -> m (HsM.Action [WithCategory a])
    withCategoryM c = fmap (withCategory c)

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> PrescriptiveDeclSpec
  -> Maybe C.OpaqueSize
  -> HsM [Hs.Decl l]
opaqueDecs info spec mSize = do
    State.modify' $ #instanceMap %~ Map.insert name insts
    env <- Reader.ask
    let decl = mkDecl env
    return $ decl : staticSizeDecls
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

    mkDecl :: HsM.Env -> Hs.Decl l
    mkDecl env = Hs.DeclEmpty Hs.EmptyData {
          name   = name
        , comment = mkHaddocks env.haddockConfig info
        , origin = Origin.Decl{
              info = info
            , kind = Origin.Opaque info.id.cName.name.kind
            , spec = spec
            }
        }

    -- We generate a 'StaticSize' instance when the (otherwise opaque) C type
    -- has a known size and alignment, that is, when a /complete/ C type was
    -- given the @emptydata@ representation.  Its methods use proxies, so the
    -- field-less data type can have the instance.
    (insts, staticSizeDecls)
      | Just (C.OpaqueSize sz al) <- mSize =
          ( Set.singleton Inst.StaticSize
          , [ Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl =
                    Hs.InstanceStaticSize name Hs.StaticSizeInstance{
                        staticSizeOf    = sz
                      , staticAlignment = al
                      }
                }
            ]
          )
      | otherwise = (Set.empty, [])

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> C.Enum Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
enumDecs info enum spec = do
    env <- Reader.ask
    nt <- newtypeDec env
    pure $ aux env nt
  where
    valueMap :: Map Integer (NonEmpty (C.FieldInfo Final, Hs.Name Hs.NsConstr))
    valueMap = Map.fromListWith (flip (<>)) [ -- preserve source order
        let name :: Hs.Name Hs.NsConstr
            name = Hs.assertNs (Proxy @Hs.NsConstr) constant.info.name.hsName
        in  (constant.value, NonEmpty.singleton (constant.info, name))
      | constant <- enum.constants
      ]

    valueNames :: Map Integer (NonEmpty String)
    valueNames = NonEmpty.map (Hs.nameToStr . snd) <$> valueMap

    mSeqBounds :: Maybe (Hs.Name Hs.NsConstr, Hs.Name Hs.NsConstr)
    mSeqBounds = do
      (minV, minNames) <- fmap (NonEmpty.map snd) <$> Map.lookupMin valueMap
      (maxV, maxNames) <- fmap (NonEmpty.map snd) <$> Map.lookupMax valueMap
      guard $ maxV - minV + 1 == fromIntegral (Map.size valueMap)
      return (NonEmpty.head minNames, NonEmpty.head maxNames)

    newtypeDec :: HsM.Env -> HsM Hs.Newtype
    newtypeDec env = do
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = enum.names.dataConstr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = enum.names.field
            , typ     = Type.topLevel enum.typ
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl{
              info = info
            , kind = Origin.Enum enum
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks env.haddockConfig info

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts env.supportedInstances.enum

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
            Just Inst.CEnum
          , Just Inst.Generic
          , Just Inst.HasCField
          , Just Inst.HasField
          , Just Inst.HasFieldCompat
          , Just Inst.HasFieldPtr
          , Just Inst.Prim
          , Just Inst.Read
          , Just Inst.ReadRaw
          , Inst.SequentialCEnum <$ mSeqBounds
          , Just Inst.Show
          , Just Inst.StaticSize
          , Just Inst.Storable
          , Just Inst.WriteRaw
          ]

    -- everything in aux is state-dependent
    aux :: HsM.Env -> Hs.Newtype -> [Hs.Decl l]
    aux env nt =
        Hs.DeclNewtype nt
        : marshalDecls
        ++ primDecl
        : optDecls
        ++ cEnumInstanceDecls
        ++ Hs.hasFieldCompatDecs nt
        ++ Hs.hasFieldPtrDecs nt
        ++ valueDecls
      where
        -- Singleton field: 'InstanceCEnum' and its siblings rely on this
        -- struct having exactly one field (the underlying enum integer).
        hsStruct :: Hs.Struct
        hsStruct = Hs.Struct {
              name      = nt.name
            , constr    = nt.constr
            , fields    = [nt.field]
            , instances = nt.instances
            , origin    = Nothing
            , comment   = Nothing
            }

        marshalDecls :: [Hs.Decl l]
        marshalDecls = [
            Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceStaticSize hsStruct.name Hs.StaticSizeInstance{
                      staticSizeOf    = enum.sizeof
                    , staticAlignment = enum.alignment
                    }
              }
          , Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceReadRaw hsStruct Hs.ReadRawInstance{
                      readRaw = Hs.Lambda (NameHint "ptr") $
                        Hs.Ap (Hs.StructCon hsStruct) [ Hs.ReadRawByteOff IZ 0 ]
                    }
              }
          , Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceWriteRaw hsStruct Hs.WriteRawInstance{
                      writeRaw = Hs.Lambda (NameHint "ptr") $ Hs.Lambda (NameHint "s") $
                        Hs.ElimStruct IZ hsStruct.constr (toNameHint nt.field.name ::: VNil) (AS AZ) $
                          Hs.Seq [ Hs.WriteRawByteOff I2 0 IZ ]
                    }
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia (Hs.EquivStorable (Hs.TypRef nt.name Nothing))
              , clss     = Inst.Storable
              , name     = nt.name
              , comment  = Nothing
              }
          ]

        primDecl :: Hs.Decl l
        primDecl = Hs.DeclDeriveInstance Hs.DeriveInstance{
              strategy = Hs.DeriveVia nt.field.typ
            , clss     = Inst.Prim
            , name     = nt.name
            , comment  = Nothing
            }

        optDecls :: [Hs.Decl l]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs env.supportedInstances.enum
          ]

        cEnumInstanceDecls :: [Hs.Decl l]
        cEnumInstanceDecls =
          let cEnumDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl =
                    Hs.InstanceCEnum hsStruct $
                      Hs.CEnumInstance nt.field.typ valueNames (isJust mSeqBounds)
                }
              cEnumShowDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl = Hs.InstanceCEnumShow hsStruct
                }
              cEnumReadDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl = Hs.InstanceCEnumRead hsStruct
                }
              sequentialCEnumDecl = case mSeqBounds of
                Just (nameMin, nameMax) -> List.singleton $
                  Hs.DeclDefineInstance Hs.DefineInstance{
                      comment      = Nothing
                    , instanceDecl =
                        Hs.InstanceSequentialCEnum hsStruct $
                          Hs.SequentialCEnumInstance nameMin nameMax
                    }
                Nothing -> []
          in  cEnumDecl : sequentialCEnumDecl ++ [cEnumShowDecl, cEnumReadDecl]

        valueDecls :: [Hs.Decl l]
        valueDecls = [
              Hs.DeclPatSyn Hs.PatSyn{
                  name    = Hs.assertNs (Proxy @Hs.NsConstr) constant.info.name.hsName
                , typ     = Hs.TypRef nt.name (Just nt.field.typ)
                , constr  = Just nt.constr
                , value   = constant.value
                , origin  = Origin.EnumConstant constant
                , comment = mkHaddocksFieldInfo env.haddockConfig info constant.info
                }
            | constant <- enum.constants
            ]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> (C.Typedef Final -> Origin.Newtype)
  -> C.Typedef Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
typedefDecs info mkNewtypeOrigin typedef spec = do
    env <- Reader.ask
    nt <- newtypeDec env
    pure $ aux env nt
  where
    newtypeDec :: HsM.Env -> HsM Hs.Newtype
    newtypeDec env = do
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = typedef.names.orig.dataConstr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = typedef.names.orig.field
            , typ     = Type.topLevel typedef.typ
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl{
              info = info
            , kind = mkNewtypeOrigin typedef
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks env.haddockConfig info

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts env.supportedInstances.typedef

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
            Inst.FromFunPtr <$ isFunType
          , Just Inst.Generic
          , Just Inst.HasCField
          , Just Inst.HasField
          , Just Inst.HasFieldCompat
          , Just Inst.HasFieldPtr
          , Inst.ToFunPtr <$ isFunType
          ]

    -- See comment in 'newtypeWrapper` below
    isFunType :: Maybe ([C.TypeFunArg Final], C.Type Final)
    isFunType = case typedef.typ of
      C.TypeFun args res | not (any C.hasUnsupportedType (res: fmap (.typ) args)) ->
        Just (args, res)
      _otherwise -> Nothing

    -- everything in aux is state-dependent
    aux :: HsM.Env -> Hs.Newtype -> [Hs.Decl l]
    aux env nt =
        Hs.DeclNewtype nt
        : newtypeWrapper
        ++ optDecls
        ++ Hs.hasFieldCompatDecs nt
        ++ Hs.hasFieldPtrDecs nt
      where
        optDecls :: [Hs.Decl l]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs env.supportedInstances.typedef
          , clss `Set.member` nt.instances
          ]

      -- We need to be careful and not generate any wrappers for function
      -- types that receive data types not supported by Haskell's FFI
      -- (i.e. structs, unions by value).
      --
      -- Note that we don't want to explicitly see all the way through
      -- typedefs here. See the following example
      --
      -- @
      -- typedef void (f)(int);
      -- typedef f g;
      -- @
      --
      -- If we see all the way through the typedef this case will not be
      -- handled correctly.
      --
        newtypeWrapper :: [Hs.Decl l]
        newtypeWrapper  = maybe [] (ToFromFunPtr.forNewtype env.sizeofs nt) isFunType

-- | Typedef around function type indirection
--
-- For example, given
--
-- > typedef void (*f)(int x, int y);
--
-- We want to generate /two/ types:
--
-- > newtype F_Aux = F_Aux (FC.CInt -> FC.CInt -> IO ())
-- > newtype F     = F (Ptr.FunPtr F_Aux)
--
-- so that @F_Aux@ can be given @ToFunPtr@/@FromFunPtr@ instances.
typedefFunTypeIndirectionDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> ([C.TypeFunArg Final], C.Type Final, C.Type Final -> C.Type Final) -- ^ Function arguments and result
  -> MangleNames.TypedefNames
  -> PrescriptiveDeclSpec
  -> HsM (HsM.Action [Hs.Decl l])
typedefFunTypeIndirectionDecs origInfo (args, res, reconstruct) names origSpec =
    fmap HsM.concatSequence $ sequence [
        HsM.delayM     $ typedefDecs auxInfo  Origin.Aux     auxTypedef  auxSpec
      , HsM.immediateM $ typedefDecs origInfo Origin.Typedef mainTypedef origSpec
      ]
  where
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1759>
    -- For historical reasons we currently implement this by making a "fake" C
    -- declaration, and then translating that immediately to Haskell. We should
    -- fuse this, and just generate the Haskell declarations directly.

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
    -- The name of this auxiliary type should be configurable.
    auxDeclIdPair :: DeclIdPair
    auxDeclIdPair = DeclIdPair{
          cName  = origInfo.id.cName
          -- Still refer to the /original/ C decl...?
        , hsName = Hs.demoteNs auxName
        }

    auxName :: Hs.Name Hs.NsTypeConstr
    auxNames  :: NewtypeNames
    (auxName, auxNames) = case names.aux of
      Nothing        -> panicPure "name of auxiliary declaration unavailable"
      Just (nm, nms) -> (nm, nms)

    auxInfo :: C.DeclInfo Final
    auxInfo = C.DeclInfo {
          loc          = origInfo.loc
        , id           = auxDeclIdPair
        , seqNr        = origInfo.seqNr
        , headerInfo   = origInfo.headerInfo
        , availability = C.Available
        , comment      = Just auxComment
        , enclosing    = []
        }

    auxComment :: C.Comment Final
    auxComment = C.Comment $ Doxy.Comment {
          brief = [
              Doxy.Text "Auxiliary type used by"
            , Doxy.Ref
                (C.CommentRef origInfo.id.cName.name.text (Just origInfo.id) Nothing)
                origInfo.id.cName.name.text
            ]
        , detailed = []
        }

    auxTypedef :: C.Typedef Final
    auxTypedef = C.Typedef{
          typ = C.TypeFun args res
        , ann = MangleNames.TypedefNames{
              orig = auxNames
            , aux  = Nothing
            }
        }

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
    -- We should think about prescriptive binding specs for _Aux types.
    auxSpec :: PrescriptiveDeclSpec
    auxSpec = PrescriptiveDeclSpec{
          cSpec  = Nothing
        , hsSpec = Nothing
        }

    mainTypedef :: C.Typedef Final
    mainTypedef = C.Typedef{
          ann = names
        , typ = reconstruct $ C.TypeTypedef $ C.Ref {
              name       = auxInfo.id
            , underlying = C.TypeFun args res
            }
        }

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     HasMacroTypes l
  => MacroLang l
  -> C.DeclInfo Final
  -> TypecheckedMacro Final l
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
macroDecs macroLang info checkedMacro spec =
    case checkedMacro of
      MacroType  ty  -> macroDecsTypedef macroLang info ty spec
      MacroValue val -> macroVarDecs info val

macroDecsTypedef ::
     forall l. (HasMacroTypes l, HasCallStack)
  => MacroLang l
  -> C.DeclInfo Final
  -> TypecheckedMacroType l Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
macroDecsTypedef macroLang info macroType spec = do
    env <- Reader.ask
    nt <- newtypeDec env
    pure $ aux env nt
  where
    newtypeDec :: HsM.Env -> HsM Hs.Newtype
    newtypeDec env = do
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.assertNs (Proxy @Hs.NsTypeConstr) info.id.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = macroType.names.dataConstr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field{
            name    = macroType.names.field
          , typ     = macroLang.translateMacroType (fmap macroVarToHsType macroType.body)
          , origin  = Origin.GeneratedField
          , comment = Nothing
          }

        macroVarToHsType :: MacroTypeBodyVar Final -> Hs.Type
        macroVarToHsType = \case
          MacroTypeExtBinding ext ->
            Hs.ExtBinding ext.hsName ext.cSpec ext.hsSpec $
              Hs.TypRef
                (Hs.assertNs (Proxy @Hs.NsTypeConstr)
                (BindingSpec.extDeclIdPair ext).hsName)
                Nothing
          MacroTypeBodyVar pair ->
            Hs.TypRef (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName) Nothing

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl {
              info = info
            , kind = Origin.Macro $ macroType
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks env.haddockConfig info

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts env.supportedInstances.typedef

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList [
              Inst.HasCField
            , Inst.HasField
            , Inst.HasFieldCompat
            , Inst.HasFieldPtr
            , Inst.Generic
            ]

    -- everything in aux is state-dependent
    aux :: HsM.Env -> Hs.Newtype -> [Hs.Decl l]
    aux env nt =
        Hs.DeclNewtype nt
        : optDecls
        ++ Hs.hasFieldCompatDecs nt
        ++ Hs.hasFieldPtrDecs nt
      where
        optDecls :: [Hs.Decl l]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs env.supportedInstances.typedef
          , clss `Set.member` nt.instances
          ]

{-------------------------------------------------------------------------------
  Globals
-------------------------------------------------------------------------------}

-- | === Global variables
--
-- For by-reference foreign imports, @capi@ vs @ccall@ makes no difference:
-- @ghc@ does not create a wrapper. For non-extern non-static globals however it
-- is important that the header is imported /somewhere/, otherwise the global
-- variable is not linked in; we therefore add an explicit import. It is
-- important that we don't import such headers more than once, but this is taken
-- care of in @csources@.
--
-- On Windows, simply generating a foreign import of a global variable's address
-- can lead to errors (see #898). For example, given a global @int
-- simpleGlobal@, the following foreign import might cause an error:
--
-- > foreign import capi safe "&simpleGlobal" simpleGlobal :: Ptr CInt
--
-- So, instead we generate a /stub/ function that simply returns the address of
-- the global variable ...
--
-- > /* get_simpleGlobal */
-- > __attribute__ ((const)) signed int *abc949ab (void) {
-- >   return &simpleGlobal;
-- > }
--
-- ... and then create a foreign import for the stub. Note that the name of the
-- stub function is mangled, though the original name of the stub function is
-- included in a comment before the stub.
--
-- > foreign import ccall unsafe "hs_bindgen_abc949ab" hs_bindgen_abc949ab :: IO (Ptr CInt)
--
-- Note that stub function also has a @const@ function attribute to emphasise
-- that the function always returns the same address throughout the lifetime of
-- the program. This means we could omit the 'IO' from the foreign import to
-- make it a pure foreign import. Instead, we make the foreign import impure and
-- we generate an additional pure Haskell function that safely unsafely runs the
-- 'IO'.
--
-- > {-# NOINLINE simpleGlobal #-}
-- > global :: Ptr CInt
-- > global = unsafePerformIO hs_bindgen_abc949ab
--
-- === Global /constant/ (i.e., @const@) variables
--
-- We generate bindings for these as we would generate bindings for non-constant
-- global variables.
--
-- However, if the type of the global constant has a 'Foreign.Storable.Storable' instance, we
-- also generate an additional \"getter\" function in Haskell land that returns
-- precisely the value of the constant rather than a /pointer/ to the value.
global ::
     C.DeclInfo Final
  -> C.Type Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl l]
global info ty _spec = do
    insts <- getInsts
    -- Generate getter if the type is @const@-qualified. We inspect the /erased/
    -- type because we want to see through newtypes as well.
    --
    -- We must have a storable instance available without any constraints.
    --
    -- We are generating a binding for a global variable here. This binding must
    -- be marked NOINLINE, so that it will be evaluated at most once. /If/ we
    -- have a Storable instance, but that storable instance has a superclass
    -- constraint, then we could _in principle_ add that superclass constraint
    -- as a constraint to the type of the global, but this would then turn the
    -- global into a function instead.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/993>
    -- We should check that the Storable instance has no superclass constraints.
    if C.isErasedTypeConstQualified ty && Inst.Storable `elem` insts then do
      (stubDecs :: [Hs.Decl l], pureStubName :: Hs.TermName) <- getStubDecsWith GlobalUniqueId
      let constGetterOfType :: [Hs.Decl l]
          constGetterOfType = constGetter (Type.topLevel ty) info pureStubName
      pure (stubDecs ++ constGetterOfType)
    -- Otherwise, do not generate a getter
    else do
      fst <$> getStubDecsWith HaskellId
  where
    getStubDecsWith :: RunnerNameSpec -> HsM ([Hs.Decl l], Hs.TermName)
    getStubDecsWith x =
      addressStubDecs info ty x _spec

    getInsts :: HsM (Set Inst.TypeClass)
    getInsts = do
        st <- State.get
        pure $ Hs.getInstances
          st.instanceMap
          Nothing
          (Set.singleton Inst.Storable)
          [Type.topLevel ty]

-- | Getter for a constant (i.e., @const@) global variable
--
-- > simpleGlobal :: CInt
-- > simpleGlobal = unsafePerformIO (peek simpleGlobal)
--
-- We only generate a getter function if the type of the global constant has a
-- 'Foreign.Storable.Storable' instance. In such cases, a user of the generated bindings should
-- use the foreign import of the stub function instead. Most notably, arrays of
-- unknown size do not have a 'Foreign.Storable.Storable' instance.
constGetter ::
     Hs.Type
  -> C.DeclInfo Final
  -> Hs.TermName
  -> [Hs.Decl l]
constGetter ty info pureStubName = singleton getterDecl
  where
    -- *** Getter ***
    --
    -- The "getter" peeks the value from the pointer
    getterDecl :: Hs.Decl l
    getterDecl = Hs.DeclVar $ Hs.Var {
          name    = Hs.ExportedName getterName
        , typ     = getterType
        , expr    = getterExpr
        , pragmas = [SHs.NOINLINE]
        , comment = Nothing
        }

    getterName = Hs.assertNs (Proxy @Hs.NsVar) info.id.hsName
    getterType = SHs.translateType ty
    getterExpr = SHs.eBindgenGlobal IO_unsafePerformIO
                `SHs.EApp` (SHs.eBindgenGlobal PtrConst_peek
                `SHs.EApp` SHs.EFree pureStubName)

data RunnerNameSpec =
      -- | The runner is public (i.e, "exported"), and we give it the human
      --   readable Haskell ID.
      HaskellId
      -- | The runner is internal (i.e., "not exported"), so we use a globally
      --   unique identifier containing a hash.
    | GlobalUniqueId

-- | Create a stub C function that returns the address of a given declaration,
-- and create a binding to that stub C function.
--
-- See 'global' and @globalConst@ for example uses.
--
-- This function returns a pair @(stubDecs, stubImportName)@:
--
-- * @stubDecs@: a list of declarations for the pure\/impure stub.
--
-- * @pureStubName@: the identifier of the /pure/ stub.
addressStubDecs ::
     C.DeclInfo Final -- ^ The given declaration
  -> C.Type Final     -- ^ The type of the given declaration
  -> RunnerNameSpec
  -> PrescriptiveDeclSpec
  -> HsM ( [Hs.Decl l]
         , Hs.TermName
         )
addressStubDecs info ty runnerNameSpec _spec = do
    env <- Reader.ask
    pure $ aux env
  where
    aux :: HsM.Env -> ([Hs.Decl l],  Hs.TermName)
    aux env = (foreignImport ++ runnerDecls, runnerName)
      where
        -- *** Stub (impure) ***
        stubImportType :: Hs.Type
        stubImportType = Hs.IO $ Type.topLevel stubType

        stubSymbol :: UniqueSymbol
        stubSymbol = globallyUnique env.uniqueId env.baseModuleName $ "get_" ++ varName

        stubName :: Hs.TermName
        stubName = Hs.InternalName stubSymbol

        varName :: String
        varName = Text.unpack $ info.id.cName.name.text

        stubType :: C.Type Final
        stubType = C.TypePointers 1 ty

        -- | The C stub return type.
        --
        -- If that type references an untagged struct\/union\/enum, then the C type
        -- can not be pretty-printed because there is no name to refer to. In such
        -- cases we use @void *@ instead, or @const void *@ if the original type was
        -- additionally const-qualified.
        cStubType :: C.Type Final
        cStubType =
              C.TypePointers 1
            $ if referencesUntagged ty
              then if C.isErasedTypeConstQualified ty
                  then C.TypeQual C.QualConst C.TypeVoid
                  else C.TypeVoid
              else ty

        prettyStub :: String
        prettyStub = concat [
              "/* ", stubSymbol.source, " */\n"
            , PC.prettyFunDefn stubDecl ""
            ]

        stubDecl :: PC.FunDefn
        stubDecl =
            PC.withArgs [] $ \args' ->
              PC.FunDefn stubSymbol.unique cStubType C.HaskellPureFunction args' $
                PC.CSList $
                PC.CSStatement
                  (PC.ExpressionStatement $ PC.Return $ PC.Address $ PC.NamedVar varName)
                  PC.CSNil

        cWrapper :: CWrapper
        cWrapper = CWrapper {
              definition     = prettyStub
            , hashIncludeArg = getMainHashIncludeArg info
            }

        foreignImport :: [Hs.Decl l]
        foreignImport =
            Hs.ForeignImport.foreignImportDec
              env.sizeofs
              (Hs.ForeignImport.FunName stubSymbol)
              []
              (Hs.ForeignImport.FunRes stubImportType)
              (uniqueCDeclName stubSymbol)
              (CallConvUserlandCapi cWrapper)
              (Origin.Global ty)
              -- These imports can be unsafe. We're binding to simple address stubs,
              -- so there are no callbacks into Haskell code. Moreover, they are
              -- short running code.
              SHs.Unsafe

        -- *** Stub (pure) ***

        runnerDecls :: [Hs.Decl l]
        runnerDecls = singleton runnerDecl

        runnerDecl :: Hs.Decl l
        runnerDecl = Hs.DeclVar $ Hs.Var {
              name    = runnerName
            , typ     = runnerType
            , expr    = runnerExpr
            , pragmas = [SHs.NOINLINE]
            , comment = mbComment <> mbUniqueSymbolComment
            }

        mbComment :: Maybe HsDoc.Comment
        mbComment = mkHaddocks env.haddockConfig info

        mbUniqueSymbolComment :: Maybe HsDoc.Comment
        mbUniqueSymbolComment = case runnerName of
          Hs.ExportedName _ -> Nothing
          Hs.InternalName x -> Just $ HsDoc.uniqueSymbol x

        name :: Hs.SomeName
        name = info.id.hsName

        uniquify :: Text -> UniqueSymbol
        uniquify = globallyUnique env.uniqueId env.baseModuleName . Text.unpack

        runnerName :: Hs.TermName
        runnerName = case runnerNameSpec of
            HaskellId      -> Hs.ExportedName $ Hs.assertNs (Proxy @Hs.NsVar) name
            GlobalUniqueId -> Hs.InternalName $ uniquify name.text

        runnerType = SHs.translateType (Type.topLevel stubType)
        runnerExpr = SHs.eBindgenGlobal IO_unsafePerformIO
                    `SHs.EApp` SHs.EFree stubName

-- | Does the C type reference an untagged struct\/union\/enum?
--
-- If so, then the C type can not be pretty-printed because there is no name to
-- refer to.
--
-- This function looks trough macro references. Macro invocations are expanded
-- by the C compiler, so whatever a macro invocation expands to (i.e., its
-- underlying type) should also not reference untagged structs\/unions\/enums.
referencesUntagged ::
     forall p. (
       HasCallStack
     , Id p ~ DeclIdPair
     , MacroId p ~ DeclIdPair
     , ExtBinding p ~ BindingSpec.ResolvedExtBinding
     , MacroUnderlying p ~ C.Type p
     )
  => C.Type p
  -> Bool
referencesUntagged = go
  where
    go :: C.Type p -> Bool
    go = \case
        C.TypePrim _pty -> False
        C.TypeComplex _pty -> False
        C.TypeRef ref ->
          -- a struct or union can be untagged
          ref.cName.isAnon
        C.TypeEnum ref ->
          -- an enum can be untagged
          ref.name.cName.isAnon
        C.TypeMacro ref
          | ref.name.cName.isAnon
          -> panicPure "macros can not be unnamed"
          -- NOTE: macros are expanded by the C preprocessor, so if pretty-print
          -- a macro name then we should make sure that it does not expand to a
          -- type that references an untagged type
          | go ref.underlying
          -> panicPure "macros can not expand to types that reference untagged types"
          | otherwise
          -> False
        C.TypeTypedef ref
          | ref.name.cName.isAnon
          -> panicPure "typedefs can not be unnamed"
          | otherwise
          -> False
        C.TypePointers _n ty -> go ty
        C.TypeConstArray _n ty -> go ty
        C.TypeIncompleteArray ty -> go ty
        C.TypeFun args res ->
          any goTypeFunArg args || go res
        C.TypeVoid -> False
        C.TypeBlock ty -> go ty
        C.TypeQual _qual ty -> go ty
        C.TypeExtBinding ref ->
          -- an external binding reference can wrap references to untagged
          -- types
          ref.name.cName.isAnon

    goTypeFunArg :: C.TypeFunArg p -> Bool
    goTypeFunArg arg = go arg.typ

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     C.DeclInfo Final
  -> TypecheckedMacroValue l Final
  -> HsM [Hs.Decl l]
macroVarDecs info macroValue = do
    env <- Reader.ask
    pure [
        Hs.DeclMacroValue $
          Hs.MacroValue
            { name    = hsVarName
            , expr    = macroValue
            , comment = mkHaddocks env.haddockConfig info
            }
      ]
  where
    hsVarName :: Hs.Name Hs.NsVar
    hsVarName = Hs.assertNs (Proxy @Hs.NsVar) info.id.hsName

{-------------------------------------------------------------------------------
  Anon Enum Constants
-------------------------------------------------------------------------------}

anonEnumConstantDecs ::
     HasCallStack
  => C.DeclInfo Final
  -> C.AnonEnumConstant Final
  -> HsM [Hs.Decl l]
anonEnumConstantDecs info anonEnumConstant = do
    env <- Reader.ask
    pure $ aux env
  where
    aux :: HsM.Env -> [Hs.Decl l]
    aux env =
        let
          patSynName :: Hs.Name Hs.NsConstr
          patSynName =
            Hs.assertNs
              (Proxy @Hs.NsConstr)
              anonEnumConstant.constant.info.name.hsName

          patSynType :: Hs.Type
          patSynType = Type.topLevel (C.TypePrim anonEnumConstant.typ)

          typeSigDecl :: Hs.Decl l
          typeSigDecl = Hs.DeclPatSyn Hs.PatSyn{
                name    = patSynName
              , typ     = patSynType
              , constr  = Nothing
              , value   = fromInteger anonEnumConstant.constant.value
              , origin  = Origin.EnumConstant anonEnumConstant.constant
              , comment = mkHaddocks env.haddockConfig info
              }
        in  [typeSigDecl]
