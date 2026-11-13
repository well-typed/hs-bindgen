-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    withInstances
  , forFunction
  , forNewtype
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport (ImportFor (..))
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.HsModule.Pretty ()
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassingMethod
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API

  NOTE: The names we generate here exist in Haskell only (they do not have
  C counterparts); it therefore suffices if they are locally unique.
-------------------------------------------------------------------------------}

-- | The function type, if it gets @ToFunPtr@ and @FromFunPtr@ instances
--
-- A function type gets instances iff it is classified and no position is
-- serialised: function pointers have no C wrapper. The @TranslateTypes@ pass
-- reports the function types without instances.
withInstances :: FunType Final -> Maybe (ClassifiedFunType Final)
withInstances = \case
    FunTypeUnclassified _ -> Nothing
    FunTypeClassified funType
      | any (isSerialized . (.ann)) funType.args -> Nothing
      | isResSerialized funType.res.ann          -> Nothing
      | otherwise                                -> Just funType

-- | Generate ToFunPtr/FromFunPtr instances for a nested function type
--
-- These instances are placed in the main module to avoid orphan instances.
forFunction :: ClassifiedFunType Final -> [Hs.Decl l]
forFunction funType =
    instancesFor
      nameTo
      nameFrom
      funType
      funHs
      importFor
  where
    funHs = hsFunType funType

    importFor :: ImportFor
    importFor = ImportForFunction {
          args = hsArgTypes funType
        , res  = Hs.IO funType.res.typ.hs
        }

    nameWith :: String -> UniqueSymbol
    nameWith s =
      locallyUnique $ "instance " <> s <> " (" <> prettyHsType funHs <> ")"

    nameTo, nameFrom :: UniqueSymbol
    nameTo   = nameWith "ToFunPtr"
    nameFrom = nameWith "FromFunPtr"

-- | Generate instances for newtype around functions
forNewtype ::
     Hs.Newtype
  -> ClassifiedFunType Final
  -> [Hs.Decl l]
forNewtype newtyp funType =
    instancesFor
      nameTo
      nameFrom
      funType
      funHs
      importFor
  where
    funHs = Hs.TypRef newtyp.name (Just newtyp.field.typ)

    importFor :: ImportFor
    importFor = ImportForNewtype {
          args   = hsArgTypes funType
        , res    = Hs.IO funType.res.typ.hs
        , newtyp = newtyp
        }

    nameWith :: String -> UniqueSymbol
    nameWith s = locallyUnique $ s <> Hs.nameToStr newtyp.name

    nameTo, nameFrom :: UniqueSymbol
    nameTo   = nameWith "to"
    nameFrom = nameWith "from"

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

instancesFor ::
     UniqueSymbol -- ^ Name of the @toFunPtr@ fun
  -> UniqueSymbol -- ^ Name of the @fromFunPtr@ fun
  -> ClassifiedFunType Final
  -> Hs.Type      -- ^ Haskell type of the instance
  -> ImportFor
  -> [Hs.Decl l]
instancesFor nameTo nameFrom funType funHs importFor = concat [
      -- import for @ToFunPtr@ instance
      HsFI.foreignImportWrapperDec
        (Hs.ForeignImport.FunName nameTo)
        funHs
        (ffiFunType funType)
        importFor
        (Origin.ToFunPtr funC)

      -- import for @FromFunPtr@ instance
    , HsFI.foreignImportDynamicDec
        (Hs.ForeignImport.FunName nameFrom)
        funHs
        (ffiFunType funType)
        importFor
        (Origin.ToFunPtr funC)

      -- @ToFunPtr@ instance proper
    , [ Hs.DeclDefineInstance Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl = Hs.InstanceToFunPtr Hs.ToFunPtrInstance{
                typ  = funHs
              , body = nameTo
              }
          }
      ]

      -- @FromFunPtr@ instance proper
    , [ Hs.DeclDefineInstance Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl = Hs.InstanceFromFunPtr Hs.FromFunPtrInstance{
                typ  = funHs
              , body = nameFrom
              }
          }
      ]
    ]
  where
    funC :: C.Type Final
    funC = C.TypeFun (map C.typeOfFunctionArg funType.args) funType.res.typ.c

-- | Haskell types of the arguments
hsArgTypes :: ClassifiedFunType Final -> [Hs.Type]
hsArgTypes funType = map (.typ.hs) funType.args

-- | Haskell type of the function type
hsFunType :: ClassifiedFunType Final -> Hs.Type
hsFunType funType = Translation.funType (hsArgTypes funType) funType.res.typ.hs

-- | FFI type of the function type
--
-- The result is in 'IO': the @"wrapper"@ and @"dynamic"@ imports run in 'IO'.
ffiFunType :: ClassifiedFunType Final -> Hs.FFIFunType
ffiFunType funType = Hs.FFIFunType{
      args = map (passingMethodFFIType . (.ann)) funType.args
    , res  = case funType.res.ann of
        PassResVoid      -> Hs.FFIResIOUnit
        PassResNonVoid x -> Hs.FFIResIO (passingMethodFFIType x)
    }

prettyHsType :: Hs.Type -> String
prettyHsType = show . PP.pretty . SHs.translateType
