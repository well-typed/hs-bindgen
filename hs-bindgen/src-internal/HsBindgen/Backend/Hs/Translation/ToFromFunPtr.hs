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
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.HsModule.Render ()
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
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
forFunction :: TranslationState -> ([C.Type Final], C.Type Final) -> [Hs.Decl]
forFunction transState (args, res) =
    instancesFor
      transState
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
     TranslationState
  -> Hs.Name Hs.NsTypeConstr
  -> ([C.Type Final], C.Type Final)
  -> [Hs.Decl]
forNewtype transState newtypeName (args, res) =
    instancesFor
      transState
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
     TranslationState
  -> UniqueSymbol -- ^ Name of the @toFunPtr@ fun
  -> UniqueSymbol -- ^ Name of the @fromFunPtr@ fun
  -> C.Type Final -- ^ Type of the C function
  -> HsType       -- ^ Corresponding Haskell type
  -> [Hs.Decl]
instancesFor transState nameTo nameFrom funC funHs = concat [
      -- import for @ToFunPtr@ instance
      HsFI.foreignImportWrapperDec
        transState
        (Hs.ForeignImport.FunName nameTo)
        funHs
        (Origin.ToFunPtr funC)

      -- import for @FromFunPtr@ instance
    , HsFI.foreignImportDynamicDec
        transState
        (Hs.ForeignImport.FunName nameFrom)
        funHs
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

-- TODO: Ideally this would live elsewhere
prettyHsType :: HsType -> String
prettyHsType =
      PP.renderCtxDoc PP.defaultContext
    . PP.pretty
    . SHs.translateType
