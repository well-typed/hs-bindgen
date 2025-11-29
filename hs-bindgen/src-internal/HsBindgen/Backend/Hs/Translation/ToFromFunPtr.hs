-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    forFunction
  , forNewtype
  ) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API
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
      (genNameFromArgs args res "to")
      (genNameFromArgs args res "from")
      (C.TypeFun args res)
      (Type.topLevel ft)
  where
    ft = C.TypeFun args res

-- | Generate instances for newtype around functions
forNewtype :: Hs.Name Hs.NsTypeConstr -> ([C.Type], C.Type) -> [Hs.Decl]
forNewtype newtypeName (args, res) =
    instancesFor
      ("to" <> coerce newtypeName)
      ("from" <> coerce newtypeName)
      (C.TypeFun args res)
      (HsTypRef newtypeName)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

instancesFor ::
     Hs.Name Hs.NsVar    -- ^ Name of the @toFunPtr@ function
  -> Hs.Name Hs.NsVar    -- ^ Name of the @fromFunPtr@ function
  -> C.Type              -- ^ Type of the C function
  -> HsType              -- ^ Corresponding Haskell type
  -> [Hs.Decl]
instancesFor nameTo nameFrom funC funHs = [
      -- import for @ToFunPtr@ instance
      Hs.DeclForeignImport Hs.ForeignImportDecl{
          foreignImportName       = nameTo
        , foreignImportResultType = NormalResultType $ HsIO (HsFunPtr funHs)
        , foreignImportParameters = [wrapperParam funHs]
        , foreignImportOrigName   = "wrapper"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.ToFunPtr funC
        , foreignImportComment    = Nothing
        , foreignImportSafety     = SHs.Safe
        }

      -- import for @FromFunPtr@ instance
    , Hs.DeclForeignImport Hs.ForeignImportDecl{
          foreignImportName       = nameFrom
        , foreignImportResultType = NormalResultType funHs
        , foreignImportParameters = [wrapperParam $ HsFunPtr funHs]
        , foreignImportOrigName   = "dynamic"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.FromFunPtr funC
        , foreignImportComment    = Nothing
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

-- | Generate a unique name for FFI stubs based on function signature
genNameFromArgs :: [C.Type] -> C.Type -> String -> Hs.Name 'Hs.NsVar
genNameFromArgs args' res' suffix =
    Hs.Name $ T.pack $ "funPtr_" ++ typeHash args' res' ++ "_" ++ suffix
  where
    typeHash :: [C.Type] -> C.Type -> String
    typeHash args res = B.unpack $ B.take 8 $ B16.encode $
      hash $ B.pack $ show (args, res)
