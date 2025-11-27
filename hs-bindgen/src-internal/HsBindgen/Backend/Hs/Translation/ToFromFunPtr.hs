-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    forFunction
  , forNewtype
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.HsModule.Render ()
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Imports
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
forFunction :: ([C.Type], C.Type) -> [Hs.Decl]
forFunction (args, res) =
    instancesFor
      (unsafeUniqueHsName nameTo   , Just $ HsDoc.uniqueSymbol nameTo)
      (unsafeUniqueHsName nameFrom , Just $ HsDoc.uniqueSymbol nameFrom)
      funC
      funHs
  where
    funC  = C.TypeFun args res
    funHs = Type.topLevel funC

    nameTo, nameFrom :: UniqueSymbol
    nameTo   = locallyUnique $ "instance ToFunPtr (" ++ prettyHsType funHs ++ ")"
    nameFrom = locallyUnique $ "instance FromFunPtr (" ++ prettyHsType funHs ++ ")"

-- | Generate instances for newtype around functions
forNewtype :: Hs.Name Hs.NsTypeConstr -> ([C.Type], C.Type) -> [Hs.Decl]
forNewtype newtypeName (args, res) =
    instancesFor
      (nameTo   , Nothing)
      (nameFrom , Nothing)
      funC
      funHs
  where
    funC  = C.TypeFun args res
    funHs = HsTypRef newtypeName

    nameTo, nameFrom :: Hs.Name Hs.NsVar
    nameTo   = "to"   <> coerce newtypeName
    nameFrom = "from" <> coerce newtypeName

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

instancesFor ::
     (Hs.Name Hs.NsVar, Maybe HsDoc.Comment) -- ^ Name of the @toFunPtr@ fun
  -> (Hs.Name Hs.NsVar, Maybe HsDoc.Comment) -- ^ Name of the @fromFunPtr@ fun
  -> C.Type                                  -- ^ Type of the C function
  -> HsType                                  -- ^ Corresponding Haskell type
  -> [Hs.Decl]
instancesFor (nameTo, nameToComment) (nameFrom, nameFromComment) funC funHs = [
      -- import for @ToFunPtr@ instance
      Hs.DeclForeignImport Hs.ForeignImportDecl{
          foreignImportName       = nameTo
        , foreignImportResultType = HsIO (HsFunPtr funHs)
        , foreignImportParameters = [wrapperParam funHs]
        , foreignImportOrigName   = "wrapper"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.ToFunPtr funC
        , foreignImportComment    = nameToComment
        , foreignImportSafety     = SHs.Safe
        }

      -- import for @FromFunPtr@ instance
    , Hs.DeclForeignImport Hs.ForeignImportDecl{
          foreignImportName       = nameFrom
        , foreignImportResultType = funHs
        , foreignImportParameters = [wrapperParam $ HsFunPtr funHs]
        , foreignImportOrigName   = "dynamic"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.FromFunPtr funC
        , foreignImportComment    = nameFromComment
        , foreignImportSafety     = SHs.Safe
        }

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
