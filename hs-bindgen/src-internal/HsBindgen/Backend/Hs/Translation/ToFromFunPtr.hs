-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    Callback(..)
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
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API

  NOTE: The names we generate here exist in Haskell only (they do not have
  C counterparts); it therefore suffices if they are locally unique.
-------------------------------------------------------------------------------}

-- | The declarations behind one callback
--
-- The two halves go to different modules. The instances stay with their type,
-- in the types module, so that they are not orphans. The code they name needs
-- @FFIType@ to reduce (hs-boot limitation/requirement), so it goes to the FFI
-- module, which the types module imports through its boot file.
--
-- See the golden fixture @types/boot/ffi_module@.
data Callback l = Callback {
      -- | The @"wrapper"@ and @"dynamic"@ imports, and the functions around them
      ffi       :: [Hs.Decl l]
      -- | The @ToFunPtr@ and @FromFunPtr@ instances, which name those functions
    , instances :: [Hs.Decl l]
    }

-- | Generate ToFunPtr/FromFunPtr instances for nested function pointer types
--
-- This function analyses the function declaration arguments and return types,
-- and for each function pointer argument/return type containing at least one
-- non-orphan type, generates the FFI wrapper and dynamic stubs along with
-- the respective ToFunPtr and FromFunPtr instances.
--
-- These instances are placed in the main module to avoid orphan instances.
forFunction ::
     ([C.TypeFunArg Final], C.Type Final)
  -> Callback l
forFunction (args, res) =
    instancesFor
      nameTo
      nameFrom
      funC
      funHs
      importFor
  where
    funC  = C.TypeFun args res
    funHs = Translation.topLevel funC

    importFor :: ImportFor
    importFor = ImportForFunction {
          args = Translation.inContext Translation.FunArg <$> args
        , res =  Hs.IO $ Translation.inContext Translation.FunRes res
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
  -> ([C.TypeFunArg Final], C.Type Final)
  -> Callback l
forNewtype newtyp (args, res) =
    instancesFor
      nameTo
      nameFrom
      funC
      funHs
      importFor
  where
    funC  = C.TypeFun args res
    funHs = Hs.TypRef newtyp.name (Just newtyp.field.typ)

    importFor :: ImportFor
    importFor = ImportForNewtype {
          args = Translation.inContext Translation.FunArg <$> args
        , res =  Hs.IO $ Translation.inContext Translation.FunRes res
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
  -> C.Type Final -- ^ Type of the C function
  -> Hs.Type      -- ^ Corresponding Haskell type
  -> ImportFor
  -> Callback l
instancesFor nameTo nameFrom funC funHs importFor = Callback{
      ffi = concat [
          -- import for @ToFunPtr@ instance
          HsFI.foreignImportWrapperDec
            (Hs.ForeignImport.FunName nameTo)
            funHs
            importFor
            (Origin.ToFunPtr funC)

          -- import for @FromFunPtr@ instance
        , HsFI.foreignImportDynamicDec
            (Hs.ForeignImport.FunName nameFrom)
            funHs
            importFor
            (Origin.ToFunPtr funC)
        ]
    , instances = [
          -- @ToFunPtr@ instance proper
          Hs.DeclDefineInstance Hs.DefineInstance{
              comment      = Nothing
            , instanceDecl = Hs.InstanceToFunPtr Hs.ToFunPtrInstance{
                  typ  = funHs
                , body = nameTo
                }
            }

          -- @FromFunPtr@ instance proper
        , Hs.DeclDefineInstance Hs.DefineInstance{
              comment      = Nothing
            , instanceDecl = Hs.InstanceFromFunPtr Hs.FromFunPtrInstance{
                  typ  = funHs
                , body = nameFrom
                }
            }
        ]
    }

prettyHsType :: Hs.Type -> String
prettyHsType = show . PP.pretty . SHs.translateType
