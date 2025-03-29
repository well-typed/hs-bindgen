module HsBindgen.Hs.NameMangler.DSL.ProduceCandidate (
    -- * Definition
    ProduceCandidate(..)
  , produceCandidate
    -- * Standard instances
  , produceCandidateDefault
  , produceCandidateHaskell
    -- * Constructing new instances
    -- ** Process C names
  , camelCaseCName
    -- ** Joining parts of names
  , prefixSnakeCase
  , prefixCamelCase
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text qualified as Text

import HsBindgen.C.AST
import HsBindgen.Hs.NameMangler.API
import HsBindgen.Imports
import HsBindgen.Hs.AST.Name

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data ProduceCandidate = ProduceCandidate {
      -- | Process C names prior to further name generation
      --
      -- See 'camelCaseCName' for an example. By default, we do nothing here.
      processCName :: Text -> Text

      -- | Add a prefix to a name
      --
      -- See 'prefixSnakeCase' and 'prefixCamelCase'
    , prefixPart :: Text -> Text -> Text

      -- | Prefix used for 'NameDatacon'
    , partDatacon :: Text

      -- | Prefix used for 'NameDecon'
    , partDecon :: Text

      -- | Suffix to be used when constructing a name for 'DeclPathCtxtPtr'
    , partCtxtPtr :: Text

      -- | Prefix used for 'NameGetter'
    , partGetter :: Text

      -- | Prefix used for 'NameBuilder'
    , partBuilder :: Text
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

produceCandidateDefault :: ProduceCandidate
produceCandidateDefault = ProduceCandidate {
      processCName  = id
    , prefixPart    = prefixSnakeCase
    , partDatacon   = ""
    , partDecon     = "un"
    , partCtxtPtr   = "Deref"
    , partGetter    = "get"
    , partBuilder   = "set"
    }

-- | Attempt to produce idiomatic Haskell names
produceCandidateHaskell :: ProduceCandidate
produceCandidateHaskell = produceCandidateDefault {
      processCName = camelCaseCName
    , prefixPart   = prefixCamelCase
    , partDatacon  = "Mk"
    }

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

produceCandidate :: forall ns m.
    Monad m
 => NameMangler' m
    -- ^ The full name mangler
    --
    -- Used when defining the /candidate/ for one 'NameSpec' in terms of the
    -- /name/ we generate for another 'NameSpec'. This implies that e.g.
    -- capitalization has been applied (types will start with a capital,
    -- independent of what the candidate was, because the name is a valid
    -- Haskell name).
 -> ProduceCandidate -> NameSpec ns -> m Text
produceCandidate nm prod = aux
  where
    aux :: NameSpec ns' -> m Text
    aux (NameVar cname) = return $
        fromCName prod cname
    aux (NameField declPath cname) =
        suffixPart prod (fromCName prod cname) <$>
          generatedName (NameTycon declPath)
    aux (NameTycon declPath) = return $
        fromDeclPath prod declPath
    aux (NameDatacon declPath) =
        prefixPart prod (partDatacon prod) <$>
          generatedName (NameTycon declPath)
    aux (NameDecon declPath) =
        prefixPart prod (partDecon prod) <$>
          generatedName (NameTycon declPath)
    aux (NameGetter declPath cname) =
        prefixPart prod (partGetter prod) <$>
          generatedName (NameField declPath cname)
    aux (NameBuilder declPath cname) =
        prefixPart prod (partBuilder prod) <$>
          generatedName (NameField declPath cname)

    -- NOTE: generated name has capitalization rules applied (see above).
    generatedName :: SingNamespace ns' => NameSpec ns' -> m Text
    generatedName spec = getHsName <$> mangle' nm spec

fromCName :: ProduceCandidate -> CName -> Text
fromCName prod = processCName prod . getCName

fromDeclPath :: ProduceCandidate -> DeclPath -> Text
fromDeclPath prod = \case
    DeclPathAnon ctxt    -> concatParts prod $ aux ctxt
    DeclPathName n _ctxt -> fromCName prod n
  where
    aux :: DeclPathCtxt -> [Text]
    aux DeclPathCtxtTop =
        []
    aux (DeclPathCtxtField struct field ctxt) = concat [
          aux ctxt
        , maybeToList (fromCName prod <$> struct)
        , [fromCName prod field]
        ]
    aux (DeclPathCtxtTypedef name) =
        [fromCName prod name]
    aux (DeclPathCtxtPtr ctxt) = concat [
          aux ctxt
        , [partCtxtPtr prod]
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary: variations on 'prefixPart'
-------------------------------------------------------------------------------}

-- | Add suffix
suffixPart :: ProduceCandidate -> Text -> Text -> Text
suffixPart = flip . prefixPart

-- | Join many names
--
-- NOTE: We cannot assume that @mempty@ is a neutral element wrt 'prefixPart',
-- so we are careful to only use it when actually needed.
concatParts :: ProduceCandidate -> [Text] -> Text
concatParts prod = go
  where
    go :: [Text] -> Text
    go []     = mempty
    go [t]    = t
    go (t:ts) = prefixPart prod t (go ts)

{-------------------------------------------------------------------------------
  Process C names
-------------------------------------------------------------------------------}

-- | Converting from @snake_case@ to @camelCase@
--
-- Leading and trailing underscores are assumed to have special meaning and
-- are preserved.  All other underscores are removed.  Letters following
-- (preserved or removed) underscores are changed to uppercase.
camelCaseCName :: Text -> Text
camelCaseCName =
    Text.pack . start False . Text.unpack
  where
    start :: Bool -> String -> String
    start isUp = \case
      c:cs
        | c == '_'  -> c : start True cs
        | otherwise -> (if isUp then Char.toUpper c else c) : aux 0 cs
      []            -> []

    aux :: Int -> String -> String
    aux !numUs = \case
      c:cs
        | c == '_'  -> aux (numUs + 1) cs
        | otherwise -> (if numUs > 0 then Char.toUpper c else c) : aux 0 cs
      []            -> List.replicate numUs '_'

{-------------------------------------------------------------------------------
  Joining names
-------------------------------------------------------------------------------}

-- | Join parts of a name with underscores (@_@)
prefixSnakeCase :: Text -> Text -> Text
prefixSnakeCase a b
  | Text.null a = b
  | otherwise   = a <> "_" <> b

-- | Join parts of a name in @camelCase@ style
--
-- The first character of all parts but the first is changed to uppercase (if it
-- is a letter), and the results are concatenated.
--
-- Since this function may change the case of letters, it can cause name
-- collisions when different C names only differ by case of the first letter.
prefixCamelCase :: Text -> Text -> Text
prefixCamelCase a b
  | Text.null a = b
  | otherwise   = a <> upperFirstChar b
  where
    upperFirstChar :: Text -> Text
    upperFirstChar t = case Text.uncons t of
      Just (c, t') -> Text.cons (Char.toUpper c) t'
      Nothing      -> t

