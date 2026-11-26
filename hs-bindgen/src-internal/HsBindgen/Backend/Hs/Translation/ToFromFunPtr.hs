-- | Generate @ToFunPtr@ and @FromFunPtr@ instances
--
-- Intended for qualified import.
--
-- > HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
module HsBindgen.Backend.Hs.Translation.ToFromFunPtr (
    forInstance
  ) where

import Data.Bifunctor (bimap)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.ForeignImport (ImportFor (..))
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.HsModule.Pretty ()
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.FunPtrInstance
import HsBindgen.IR.FunSig
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API

  NOTE: The names we generate here exist in Haskell only (they do not have
  C counterparts); it therefore suffices if they are locally unique.
-------------------------------------------------------------------------------}

-- | Generate a pair of @ToFunPtr@ and @FromFunPtr@ instances, with their
-- @"wrapper"@ and @"dynamic"@ imports
forInstance :: FunPtrInstance -> [Hs.Decl l]
forInstance inst = concat [
      -- import for @ToFunPtr@ instance
      HsFI.foreignImportWrapperDec
        (HsFI.FunName nameTo)
        instHead
        ffiFunType
        importFor
        (Origin.ToFunPtr instHead)

      -- import for @FromFunPtr@ instance
    , HsFI.foreignImportDynamicDec
        (HsFI.FunName nameFrom)
        instHead
        ffiFunType
        importFor
        (Origin.FromFunPtr instHead)

      -- @ToFunPtr@ instance proper
    , [ Hs.DeclDefineInstance Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl = Hs.InstanceToFunPtr Hs.ToFunPtrInstance{
                typ  = instHead
              , body = nameTo
              }
          }
      ]

      -- @FromFunPtr@ instance proper
    , [ Hs.DeclDefineInstance Hs.DefineInstance{
            comment      = Nothing
          , instanceDecl = Hs.InstanceFromFunPtr Hs.FromFunPtrInstance{
                typ  = instHead
              , body = nameFrom
              }
          }
      ]
    ]
  where
    funType :: Hs.Type
    funType = Translation.funType $ bimap (.hs) (.hs) inst.sig

    -- The type of the instances
    instHead :: Hs.Type
    instHead = case inst.head of
      HeadFunType        -> funType
      HeadNewtype name _ -> Hs.TypRef name

    importFor :: ImportFor
    importFor = case inst.head of
      HeadFunType         -> ImportForFunction hsSig
      HeadNewtype _ names -> ImportForNewtype hsSig names

    hsSig :: FunSig Hs.Type Hs.Type
    hsSig = bimap (.hs) (Hs.IO . (.hs)) inst.sig

    ffiFunType :: Hs.FFIFunType
    ffiFunType = bimap (.ffi) (.ffi) inst.sig

    nameTo, nameFrom :: UniqueSymbol
    (nameTo, nameFrom) = case inst.head of
      HeadFunType -> (
          funTypeName "ToFunPtr"
        , funTypeName "FromFunPtr"
        )
      HeadNewtype name _ -> (
          locallyUnique $ "to"   <> Hs.nameToStr name
        , locallyUnique $ "from" <> Hs.nameToStr name
        )

    funTypeName :: String -> UniqueSymbol
    funTypeName s =
      locallyUnique $ "instance " <> s <> " (" <> prettyHsType funType <> ")"

prettyHsType :: Hs.Type -> String
prettyHsType = show . PP.pretty . SHs.translateType
