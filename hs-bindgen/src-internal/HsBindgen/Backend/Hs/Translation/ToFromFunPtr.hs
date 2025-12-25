-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    forFunction
  , forNewtype
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.HsModule.Render ()
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API

  NOTE: The names we generate here exist in Haskell only (they do not have
  C counterparts); it therefore suffices if they are locally unique.
-------------------------------------------------------------------------------}

-- | Generate ToFunPtr/FromFunPtr instances for nested function pointer types
--
-- This function analyses the function declaration arguments and return types,
-- and for each function pointer argument/return type containing at least one
-- non-orphan type, generates the FFI wrapper and dynamic stubs along with
-- the respective ToFunPtr and FromFunPtr instances.
--
-- These instances are placed in the main module to avoid orphan instances.
forFunction :: ([C.Type Final], C.Type Final) -> [Hs.Decl]
forFunction (args, res) =
    instancesFor
      nameTo
      nameFrom
      funC
      funHs
  where
    funC  = C.TypeFun args res
    funHs = Type.topLevel funC

    nameWith :: String -> UniqueSymbol
    nameWith s =
      locallyUnique $ "instance " <> s <> " (" <> prettyHsType funHs <> ")"

    nameTo, nameFrom :: UniqueSymbol
    nameTo   = nameWith "ToFunPtr"
    nameFrom = nameWith "FromFunPtr"

-- | Generate instances for newtype around functions
forNewtype ::
     Hs.Name Hs.NsTypeConstr
  -> ([C.Type Final], C.Type Final)
  -> [Hs.Decl]
forNewtype newtypeName (args, res) =
    instancesFor
      nameTo
      nameFrom
      funC
      funHs
  where
    funC  = C.TypeFun args res
    funHs = HsTypRef newtypeName

    nameWith :: String -> UniqueSymbol
    nameWith s = locallyUnique $ s <> Text.unpack (Hs.getName newtypeName)

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
  -> HsType       -- ^ Corresponding Haskell type
  -> [Hs.Decl]
instancesFor nameTo nameFrom funC funHs = [
      -- import for @ToFunPtr@ instance
      HsFI.foreignImportDec
        (Hs.InternalName nameTo)
        (HsIO (HsFunPtr funHs))
        [wrapperParam funHs]
        (C.DeclName "wrapper" C.NameKindOrdinary)
        (CallConvGhcCCall ImportAsValue)
        (Origin.ToFunPtr funC)
        SHs.Safe

      -- import for @FromFunPtr@ instance
    , HsFI.foreignImportDec
        (Hs.InternalName nameFrom)
        funHs
        [wrapperParam $ HsFunPtr funHs]
        (C.DeclName "dynamic" C.NameKindOrdinary)
        (CallConvGhcCCall ImportAsValue)
        (Origin.ToFunPtr funC)
        SHs.Safe

      -- @ToFunPtr@ instance proper
    , Hs.DeclDefineInstance Hs.DefineInstance{
          defineInstanceComment      = Nothing
        , defineInstanceDeclarations = Hs.InstanceToFunPtr
            Hs.ToFunPtrInstance{
                toFunPtrInstanceType = funHs
              , toFunPtrInstanceBody = nameTo
              }
        }

      -- @FromFunPtr@ instance proper
    , Hs.DeclDefineInstance Hs.DefineInstance{
          defineInstanceComment      = Nothing
        , defineInstanceDeclarations = Hs.InstanceFromFunPtr
            Hs.FromFunPtrInstance{
                fromFunPtrInstanceType = funHs
              , fromFunPtrInstanceBody = nameFrom
              }
        }
    ]

wrapperParam :: HsType -> Hs.FunctionParameter
wrapperParam hsType = Hs.FunctionParameter{
      functionParameterName    = Nothing
    , functionParameterType    = hsType
    , functionParameterComment = Nothing
    }

-- TODO: Ideally this would live elsewhere
prettyHsType :: HsType -> String
prettyHsType =
      PP.renderCtxDoc PP.defaultContext
    . PP.pretty
    . SHs.translateType
