{-# LANGUAGE RecordWildCards #-}

-- | Doxygen XML parser — internal implementation
--
-- This module contains the full implementation of the Doxygen XML parser.
-- The public API is re-exported by "Doxygen.Parser"; this module additionally
-- exposes internal functions for testing.
--
-- === What doxygen generates
--
-- Given a C header, @doxygen@ produces an @xml\/@ directory containing:
--
-- [@index.xml@]
--   Master index of all "compounds" (files, groups, structs, unions) and their
--   members.  Each member's @refid@ attribute encodes which group it belongs
--   to, e.g. @\"group__core__types_1ga...\"@ means the member is in the
--   @core_types@ group, while @\"myheader_8h_1a...\"@ means ungrouped.
--
-- [@\<file\>_8h.xml@]
--   One per input file (e.g. @myheader_8h.xml@).  Lists all declarations
--   with their @refid@s — we don't parse comments from these directly.
--
-- [@group__\<name\>.xml@]
--   One per @\@defgroup@\/@\@addtogroup@ group.  Contains the group title,
--   member comments, inner-group references, and enum value docs.
--
-- [@struct\<name\>.xml@ \/ @union\<name\>.xml@]
--   One per struct\/union.  Contains the compound-level comment and
--   per-field comments (brief + detailed).
--
-- We parse @index.xml@ first for the member→group mapping, then iterate
-- over all other @.xml@ files to extract comments.  Each file is parsed
-- into a 'XMLFileResult' and the results are merged into 'Doxygen'.
--
module Doxygen.Parser.Internal (
    -- * Configuration
    Config(..)
  , defaultConfig
    -- * Parsing
  , parse
  , Result(..)
    -- * State type
  , Doxygen(..)
  , emptyDoxygen
    -- * Lookup keys
  , DoxygenKey(..)
  , lookupComment
    -- * Group sections
  , lookupGroupMembership
  , lookupGroupInfo
    -- * Errors
  , DoxygenException(..)
    -- * Internals (exported for testing)
  , buildComment
  , extractBriefAndDetail
  , parseBlockElement
  , parseInline
  , parseInlineChildren
  , normalizeWhitespace
  , trimEdges
  , XMLFileResult(..)
  , extractEntity
  , nodeElementName
  , forChildren
  , ChildAction(..)
  , readXML
  , parseXMLOutput
  ) where

import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad (unless)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), proc,
                       readCreateProcessWithExitCode)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor, ($/), ($//))
import Text.XML.Cursor qualified as Cursor

import Doxygen.Parser.Types
import Doxygen.Parser.Warning

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Configuration for the doxygen invocation
--
data Config = Config {
    binary     :: FilePath
    -- ^ Path to doxygen executable (default: @\"doxygen\"@)
  , extractAll :: Bool
    -- ^ @EXTRACT_ALL@ option: document all entities even without
    -- Doxygen comments (default: @True@)
  , quiet      :: Bool
    -- ^ @QUIET@ option: suppress doxygen output (default: @True@)
  , outputDir  :: Maybe FilePath
    -- ^ When @Just dir@, doxygen XML output is written to @dir@
    -- and persists after 'parse' returns.  When 'Nothing' (default),
    -- a temporary directory is used and cleaned up automatically.
  }
  deriving stock (Show, Eq)

-- | Default configuration: uses @\"doxygen\"@ from @PATH@, extracts all,
-- quiet mode enabled, temporary output directory
--
defaultConfig :: Config
defaultConfig = Config {
    binary     = "doxygen"
  , extractAll = True
  , quiet      = True
  , outputDir  = Nothing
  }

{-------------------------------------------------------------------------------
  Lookup keys
-------------------------------------------------------------------------------}

-- | Key for looking up a comment in the 'Doxygen' state
--
-- Unifies the four separate map lookups (declarations, structs, fields,
-- enum values) into a single 'Map' keyed by this type.
--
data DoxygenKey
  = KeyDecl { name :: Text }
    -- ^ Function, typedef, variable, or enum (keyed by C name)
  | KeyStruct { name :: Text }
    -- ^ Struct\/union compound (keyed by C type name)
  | KeyField { structName :: Text, fieldName :: Text }
    -- ^ Struct\/union field
  | KeyEnumValue { enumName :: Text, valueName :: Text }
    -- ^ Enum value
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | Parsed doxygen state for C\/C++ headers
--
-- The 'comments' map uses 'DoxygenKey' to distinguish declaration-level,
-- struct-level, field-level, and enum-value-level comments.
-- The @Comment Text@ values contain @Text@ at cross-reference
-- positions (@\<ref\>@ elements in the XML). These @Text@ values are
-- C names that consumers can resolve to their own identifier types
-- via 'Functor'\/'Traversable' on 'Comment'.
--
data Doxygen = Doxygen {
    -- | All extracted comments, keyed by 'DoxygenKey'.
    comments        :: Map DoxygenKey (Comment Text)
    -- | Group membership: declaration name -> group name.
    --
    -- Useful for generating export-list sections from Doxygen
    -- @\@defgroup@ annotations.
  , groupMembership :: Map Text Text
    -- | Group info: group name -> (title, parent group name).
    --
    -- Useful for generating export-list sections from Doxygen
    -- @\@defgroup@ annotations.
  , groupInfo       :: Map Text (Text, Maybe Text)
  }
  deriving stock (Show)

-- | Empty doxygen state (no comments, no group information).
--
-- Useful as a fallback when doxygen is unavailable or fails, so that
-- downstream code can proceed without documentation rather than aborting.
emptyDoxygen :: Doxygen
emptyDoxygen = Doxygen {
    comments        = Map.empty
  , groupMembership = Map.empty
  , groupInfo       = Map.empty
  }

{-------------------------------------------------------------------------------
  Lookups
-------------------------------------------------------------------------------}

-- | Look up a comment by key
lookupComment :: DoxygenKey -> Doxygen -> Maybe (Comment Text)
lookupComment key doxy = Map.lookup key doxy.comments

-- | Look up which group a declaration belongs to
lookupGroupMembership :: Text -> Doxygen -> Maybe Text
lookupGroupMembership declName doxy =
  Map.lookup declName doxy.groupMembership

-- | Look up group info: @(title, parent group name)@
lookupGroupInfo :: Text -> Doxygen -> Maybe (Text, Maybe Text)
lookupGroupInfo groupName doxy =
  Map.lookup groupName doxy.groupInfo

{-------------------------------------------------------------------------------
  Result
-------------------------------------------------------------------------------}

-- | Result of parsing doxygen XML output
data Result = Result {
    doxygen        :: Doxygen
    -- ^ The parsed comments and group information
  , warnings       :: [Warning]
    -- ^ Parser warnings about unsupported or degraded content.
    -- These are non-fatal issues detected during XML parsing (e.g.,
    -- unsupported HTML elements).  For fatal errors, see 'DoxygenException'.
  , doxygenVersion :: Text
    -- ^ The doxygen version that produced the XML (e.g., @\"1.15.0\"@).
    -- Defaults to @\"unknown\"@ if the version could not be determined.
  }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Exception thrown when doxygen invocation or XML parsing fails
data DoxygenException
  = DoxygenNotFound
    -- ^ The @doxygen@ binary was not found on @PATH@
  | DoxygenFailed Int String
    -- ^ @doxygen@ exited with a non-zero exit code.
    -- The 'Int' is the exit code; the 'String' is stderr output from the
    -- @doxygen@ process.
  | DoxygenXMLParseError FilePath String
    -- ^ Failed to parse a doxygen XML output file.
    -- The 'String' is the parse error as reported by @xml-conduit@.
  | DoxygenIOError IOError
    -- ^ An IO error occurred while invoking @doxygen@ (e.g., permission
    -- denied).  The 'DoxygenNotFound' constructor handles the specific case
    -- where the binary is absent; this covers all other 'IOError's.
  | DoxygenOutputDirMissing FilePath
    -- ^ The 'outputDir' specified in 'Config' does not exist.
  deriving stock (Show)

instance Exception DoxygenException

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Parse C\/C++ header files using doxygen
--
-- Invokes the @doxygen@ binary on the given header files, parses the XML
-- output, and returns a 'Result' containing all extracted comments, group
-- information, and any warnings about unsupported content.
--
-- When 'outputDir' is @Just dir@ in the 'Config', doxygen writes its XML
-- output to @dir\/xml\/@ and the files persist after this function returns.
-- When 'Nothing', a temporary directory is used and cleaned up automatically.
--
-- Throws 'DoxygenException' on failure.
--
parse :: Config -> NonEmpty FilePath -> IO Result
parse config headerPaths =
    withDir $ \dir -> do
      runDoxygen config headerPaths dir
      parseXMLOutput (dir </> "xml")
  where
    withDir :: (FilePath -> IO a) -> IO a
    withDir = case config.outputDir of
      Nothing  -> withSystemTempDirectory "doxygen-parser"
      Just dir -> \f -> do
        exists <- doesDirectoryExist dir
        unless exists $ throwIO $ DoxygenOutputDirMissing dir
        f dir

{-------------------------------------------------------------------------------
  Doxygen invocation
-------------------------------------------------------------------------------}

-- | Run the @doxygen@ binary, writing XML output to @outputDir\/xml\/@
runDoxygen :: Config -> NonEmpty FilePath -> FilePath -> IO ()
runDoxygen config headerPaths dir = do
    let doxyConfig = generateConfig config headerPaths dir
    (exitCode, _stdout, stderr) <- readCreateProcessWithExitCode
      (proc config.binary ["-"]) { std_in = CreatePipe }
      (Text.unpack doxyConfig)
      `catch` \e ->
        if isDoesNotExistError e
          then throwIO DoxygenNotFound
          else throwIO (DoxygenIOError e)
    case exitCode of
      ExitFailure code -> throwIO $ DoxygenFailed code stderr
      ExitSuccess      -> pure ()

-- | Generate the doxygen configuration string
generateConfig :: Config -> NonEmpty FilePath -> FilePath -> Text
generateConfig config inputPaths outputDir = Text.unlines $
  [ "INPUT             = " <> Text.unwords (map Text.pack (toList inputPaths))
  , "OUTPUT_DIRECTORY  = " <> Text.pack outputDir
  , "GENERATE_XML      = " <> boolOption True
  , "GENERATE_HTML     = " <> boolOption False
  , "GENERATE_LATEX    = " <> boolOption False
  , "XML_PROGRAMLISTING = " <> boolOption False
  , "EXTRACT_ALL       = " <> boolOption config.extractAll
  , "JAVADOC_BANNER    = " <> boolOption True
  , "QUIET             = " <> boolOption config.quiet
  ]
  where
    boolOption :: Bool -> Text
    boolOption True  = "YES"
    boolOption False = "NO"

{-------------------------------------------------------------------------------
  XML parsing — assembles Doxygen directly from XML files
-------------------------------------------------------------------------------}

-- | Parse the XML output directory into a 'Result'
--
parseXMLOutput :: FilePath -> IO Result
parseXMLOutput xmlDir = do
  -- Parse index.xml for declaration→group mappings and doxygen version
  (indexWarns, memberGroupMap, version) <- parseIndexXML (xmlDir </> "index.xml")

  -- Parse all per-entity XML files (groups, structs, unions, file) and
  -- accumulate results
  xmlFiles <- List.sort . filter isEntityXML <$> listDirectory xmlDir
  results <- mapM (\f -> parseEntityXML (xmlDir </> f)) xmlFiles

  -- Merge results from all XML files into a single state.
  -- Left-biased: if two XML files define the same key, the first one wins.
  -- This is fine because doxygen produces at most one comment per entity
  -- per run.
  let allComments   = Map.unionsWith const [r.comments | r <- results]

      -- child→parent map from <innergroup> elements
      childToParent = Map.fromList $ concatMap (.groupChildren) results

      allGroupInfo  = Map.fromList
        [ (gname, (title, Map.lookup gname childToParent))
        | r <- results
        , (gname, title) <- r.groupTitles
        ]
      innerClassMembers = Map.fromList $ concatMap (.groupMembers) results

      allGroupMembership = Map.union memberGroupMap innerClassMembers

      allWarnings   = concatMap (.warnings) results

  pure Result {
      doxygen = Doxygen {
          comments        = allComments
        , groupMembership = allGroupMembership
        , groupInfo       = allGroupInfo
        }
    , warnings        = indexWarns ++ allWarnings
    , doxygenVersion  = version
    }

-- | Intermediate result from parsing one XML file
--
-- Each XML file (group, struct, union, file) produces one of these.
-- They are merged into the final 'Doxygen' by 'parseXMLOutput'.
--
data XMLFileResult = XMLFileResult {
    comments       :: Map DoxygenKey (Comment Text)
  , groupTitles    :: [(Text, Text)]
    -- ^ (group name, title) — only populated for group XML files
  , groupChildren  :: [(Text, Text)]
    -- ^ (child group name, parent group name) — derived from
    -- @\<innergroup\>@ elements in group XML files
  , groupMembers   :: [(Text, Text)]
    -- ^ (declaration name, group name) — derived from
    -- @\<innerclass\>@ elements in group XML files (structs\/unions)
  , warnings       :: [Warning]
  }

{-------------------------------------------------------------------------------
  index.xml parsing

  The index lists every compound and its members.  Example:

    <doxygenindex>
      <compound refid="myheader_8h" kind="file"><name>myheader.h</name>
        <member refid="group__core__types_1ga..." kind="typedef">
          <name>my_int</name>
        </member>
        <member refid="myheader_8h_1a..." kind="function">
          <name>ungrouped_func</name>
        </member>
      </compound>
      <compound refid="group__core__types" kind="group">
        <name>core_types</name>
        ...
      </compound>
    </doxygenindex>

  We extract two things:
  1. member→group mapping: parse "group__core__types" from the refid prefix
     to learn that "my_int" belongs to group "core_types".  Members whose
     refid starts with the file name (e.g. "myheader_8h_1a...") are ungrouped.
  2. group refs: which group compounds exist (titles come later from compound
     XML files).
-------------------------------------------------------------------------------}

-- | Parse @index.xml@ to extract the declaration→group mapping and version.
parseIndexXML :: FilePath -> IO ([Warning], Map Text Text, Text)
parseIndexXML path = do
  doc <- readXML path
  let root    = Cursor.fromDocument doc
      version = fromMaybe "unknown"
              $ listToMaybe
              $ Cursor.attribute "version" root

      (rootWarns, compounds) =
        forChildren "doxygenindex" (Cursor.child root) $
          \n c -> case n of
            "compound" -> Just (Yield c)
            _          -> Nothing

      (compoundWarns, memberGroups) = collectWarnings $ map processCompound compounds

  pure ( rootWarns ++ compoundWarns
       , Map.fromList memberGroups
       , version
       )
  where
    processCompound :: Cursor -> ([Warning], [(Text, Text)])
    processCompound c = case listToMaybe $ Cursor.attribute "kind" c of
      Just "file" ->
        let (warns, members) =
              forChildren "compound" (Cursor.child c) $
                \n ch -> case n of
                  "member" -> Just (Yield ch)
                  "name"   -> Just Skip
                  _        -> Nothing
            (memberWarns, pairs) = collectWarnings $ map processMember members
         in (warns ++ memberWarns, pairs)
      _otherwise -> ([], [])

    processMember :: Cursor -> ([Warning], [(Text, Text)])
    processMember c =
      let (warns, names) =
            forChildren "member" (Cursor.child c) $
              \n ch -> case n of
                "name" -> Just (Yield (extractText ch))
                _      -> Nothing
          result = do
            name  <- listToMaybe names
            refid <- listToMaybe $ Cursor.attribute "refid" c
            group <- groupFromRefid refid
            pure (name, group)
       in (warns, maybeToList result)

-- | Extract group name from a refid like @\"group__core__types_1ga...\"@
--
-- Uses 'Text.breakOnEnd' to find the /last/ @\"_1\"@ (the member separator),
-- so that group names containing @\"_1\"@ as a substring are handled correctly
-- (e.g., @group__v1__helpers_1ga...@ → @\"v1_helpers\"@).
groupFromRefid :: Text -> Maybe Text
groupFromRefid refid
  | Just rest <- Text.stripPrefix "group__" refid
  = let (before, after) = Text.breakOnEnd "_1" rest
    in  Just $ Text.replace "__" "_" $
        if Text.null before || Text.null after
           then rest
           else Text.dropEnd 2 before
  | otherwise
  = Nothing

{-------------------------------------------------------------------------------
  Per-entity XML parsing

  Each entity XML file has the structure:

    <doxygen>
      <compounddef kind="group|struct|union|file">
        <compoundname>      → entity name
        <title>             → group title (groups only)
        <briefdescription>  → compound-level brief
        <detaileddescription>
        <innergroup>        → group hierarchy (groups only)
        <sectiondef kind="func|enum|typedef|public-attrib|...">
          <memberdef kind="function|enum|variable|typedef|...">
            <name>          → member name
            <qualifiedname> → e.g. "config_t::id" (struct fields)
            <briefdescription>
            <detaileddescription>
            <enumvalue>     → per-enumerator (enum memberdefs only)
              <name>
              <briefdescription>
              <detaileddescription>
            + type, definition, argsstring, param, location, ... (ignored)
          + header, description (ignored)
        + includes, location, listofallmembers, graphs, ... (ignored)

  See 'classifyCompoundDef', 'classifyMemberDef', and 'classifyEnumValue'
  for the full list of known-but-ignored elements at each level.
-------------------------------------------------------------------------------}

-- | Parse a single XML file (group, struct, union, or file)
parseEntityXML :: FilePath -> IO XMLFileResult
parseEntityXML path = do
  doc <- readXML path
  let root = Cursor.fromDocument doc
      (rootWarns, compoundDefs) =
        forChildren "doxygen" (Cursor.child root) $
          \n c -> case n of
            "compounddef" -> Just (Yield c)
            _             -> Nothing
      results = map extractEntity compoundDefs

  pure XMLFileResult {
      comments      = Map.unionsWith const [r.comments | r <- results]
    , groupTitles   = concatMap (.groupTitles) results
    , groupChildren = concatMap (.groupChildren) results
    , groupMembers  = concatMap (.groupMembers) results
    , warnings      = rootWarns ++ concatMap (.warnings) results
    }

-- | Extract all information from a @\<compounddef\>@ element.
--
-- A compounddef represents one documented entity (struct, union, group, or
-- file).  This function classifies its children, extracts comments from
-- members, and assembles a 'XMLFileResult' with all comment map entries.
--
extractEntity :: Cursor -> XMLFileResult
extractEntity cd =
  let kind     = Text.concat $ Cursor.attribute "kind" cd
      children = Cursor.child cd

      -- Classify <compounddef> children into buckets
      parts = classifyCompoundDef children

      -- Compound-level comment (struct/union docs)
      (commentWarns, mComment) = extractBriefAndDetail parts.briefs parts.details
      structDocs = case (kind `elem` ["struct", "union"], parts.name, mComment) of
        (True, Just sname, Just comment) -> [(KeyStruct sname, comment)]
        _                                -> []

      -- Group metadata (only for kind="group")
      groupTitles = case (kind, parts.name, parts.title) of
        ("group", Just gname, Just gtitle) -> [(gname, Text.strip gtitle)]
        _                                  -> []
      groupChildren = case (kind, parts.name) of
        ("group", Just parentName) ->
          [ (childName, parentName)
          | c <- parts.innerGroups
          , refid <- Cursor.attribute "refid" c
          , childName <- maybeToList (groupFromRefid refid)
          ]
        _ -> []

      groupMembers = case (kind, parts.name) of
        ("group", Just groupName) ->
          [ (extractText c, groupName)
          | c <- parts.innerClasses
          ]
        _ -> []

      -- Member comments from <sectiondef>s → keyed as KeyDecl / KeyField
      (membersWarns, members) = extractAllMembers parts.sections
      declDocs  = toDeclMap members
      fieldDocs = toFieldMap kind parts.name members

      -- Enum value comments → keyed as KeyEnumValue
      (enumWarns, enumDocs) = extractAllEnumValues members

  in XMLFileResult {
         comments      = Map.fromList (structDocs ++ declDocs ++ fieldDocs ++ enumDocs)
       , groupTitles   = groupTitles
       , groupChildren = groupChildren
       , groupMembers  = groupMembers
       , warnings      = parts.warnings ++ commentWarns ++ membersWarns ++ enumWarns
       }
  where

    {-------------------------------------------------------------------
      Comment map construction
    -------------------------------------------------------------------}

    -- Top-level declarations (functions, typedefs, enums).
    -- Struct fields have a qualifiedname containing "::" — exclude those.
    toDeclMap :: [MemberInfo] -> [(DoxygenKey, Comment Text)]
    toDeclMap ms =
      [ (KeyDecl mi.miName, mi.miComment)
      | mi <- ms
      , not (isStructField mi.miQualName) ]

    -- Struct/union field comments.
    -- For struct/union compounds, all members are fields of that struct.
    -- For other compounds (groups, files), fields are identified by their
    -- <qualifiedname> (e.g. "config_t::id" → KeyField "config_t" "id").
    toFieldMap :: Text -> Maybe Text -> [MemberInfo] -> [(DoxygenKey, Comment Text)]
    toFieldMap k mEntityName ms
      | k `elem` ["struct", "union"]
      , Just sname <- mEntityName
      = [(KeyField sname mi.miName, mi.miComment) | mi <- ms]
      | otherwise
      = [ (KeyField sname fname, mi.miComment)
        | mi <- ms
        , Just qn <- [mi.miQualName]
        , (sname, fname) <- maybeToList (splitQualifiedName qn)
        ]

    {-------------------------------------------------------------------
      <sectiondef> → <memberdef> extraction
    -------------------------------------------------------------------}

    extractAllMembers :: [Cursor] -> ([Warning], [MemberInfo])
    extractAllMembers = collectWarnings . map extractSectionMembers

    -- A sectiondef groups memberdefs by kind (func, typedef, public-attrib, …)
    extractSectionMembers :: Cursor -> ([Warning], [MemberInfo])
    extractSectionMembers secCursor =
        let (warns, memberDefs) = forChildren "sectiondef" (Cursor.child secCursor) $ \n c ->
              case n of
                "memberdef"   -> Just (Yield c)
                "header"      -> Just Skip
                "description" -> Just Skip
                _             -> Nothing
            pairs = map extractMember memberDefs
        in  (warns ++ concatMap fst pairs, mapMaybe snd pairs)

    extractMember :: Cursor -> ([Warning], Maybe MemberInfo)
    extractMember mc =
        let mp = classifyMemberDef (Cursor.child mc)
            (commentWarns, mComment) = extractBriefAndDetail mp.briefs mp.details
        in case (mp.name, mComment) of
             (Just mname, Just comment) ->
               ( mp.warnings ++ commentWarns
               , Just MemberInfo { miName = mname, miComment = comment
                                 , miQualName = mp.qualifiedName
                                 , miEnumValues = mp.enumValues }
               )
             _ -> (mp.warnings ++ commentWarns, Nothing)

    {-------------------------------------------------------------------
      <enumvalue> extraction
    -------------------------------------------------------------------}

    extractAllEnumValues :: [MemberInfo] -> ([Warning], [(DoxygenKey, Comment Text)])
    extractAllEnumValues = collectWarnings . map extractEnumValuesFrom

    extractEnumValuesFrom :: MemberInfo -> ([Warning], [(DoxygenKey, Comment Text)])
    extractEnumValuesFrom mi =
        collectWarnings $ map (extractEnumValue mi.miName) mi.miEnumValues

    extractEnumValue :: Text -> Cursor -> ([Warning], [(DoxygenKey, Comment Text)])
    extractEnumValue ename ev =
        let evp = classifyEnumValue (Cursor.child ev)
            (commentWarns, mComment) = extractBriefAndDetail evp.briefs evp.details
        in case (evp.name, mComment) of
             (Just vname, Just comment) ->
               (evp.warnings ++ commentWarns, [(KeyEnumValue ename vname, comment)])
             _ ->
               (evp.warnings ++ commentWarns, [])

    {-------------------------------------------------------------------
      Child classifiers — one fold per XML element type

      Each classifier iterates children exactly once, dispatching on
      element name.  Known-but-ignored elements are listed explicitly
      so that only truly unknown elements trigger warnings.
    -------------------------------------------------------------------}

    classifyCompoundDef :: [Cursor] -> CompoundParts
    classifyCompoundDef = foldr go (CompoundParts [] Nothing Nothing [] [] [] [] [])
      where
        go c acc@CompoundParts{..} = case nodeElementName c of
          -- Processed:
          Just "compoundname"        -> CompoundParts { name = Just (extractText c), .. }
          Just "title"               -> CompoundParts { title = Just (extractText c), .. }
          Just "briefdescription"    -> CompoundParts { briefs = c : briefs, .. }
          Just "detaileddescription" -> CompoundParts { details = c : details, .. }
          Just "sectiondef"          -> CompoundParts { sections = c : sections, .. }
          Just "innergroup"          -> CompoundParts { innerGroups = c : innerGroups, .. }
          Just "innerclass"          -> CompoundParts { innerClasses = c : innerClasses, .. }
          -- Known but ignored (expected doxygen output):
          Just "includes"            -> acc
          Just "includedby"          -> acc
          Just "innernamespace"      -> acc
          Just "listofallmembers"    -> acc
          Just "location"            -> acc
          Just "collaborationgraph"  -> acc
          Just "inheritancegraph"    -> acc
          Just "incdepgraph"         -> acc
          Just "invincdepgraph"      -> acc
          Just "templateparamlist"   -> acc
          Just "basecompoundref"     -> acc
          Just "derivedcompoundref"  -> acc
          Just "programlisting"      -> acc
          -- Unknown → warn
          Just other -> CompoundParts
            { warnings = unknownChildWarning "compounddef" other : warnings, .. }
          Nothing -> acc

    classifyMemberDef :: [Cursor] -> MemberParts
    classifyMemberDef = foldr go (MemberParts [] Nothing Nothing [] [] [])
      where
        go c acc@MemberParts{..} = case nodeElementName c of
          Just "name"                 -> MemberParts { name = Just (extractText c), .. }
          Just "qualifiedname"        -> MemberParts { qualifiedName = Just (extractText c), .. }
          Just "briefdescription"     -> MemberParts { briefs = c : briefs, .. }
          Just "detaileddescription"  -> MemberParts { details = c : details, .. }
          Just "enumvalue"            -> MemberParts { enumValues = c : enumValues, .. }
          -- Known but ignored:
          Just "type"                 -> acc
          Just "definition"           -> acc
          Just "argsstring"           -> acc
          Just "location"             -> acc
          Just "param"                -> acc
          Just "initializer"          -> acc
          Just "reimplements"         -> acc
          Just "reimplementedby"      -> acc
          Just "references"           -> acc
          Just "referencedby"         -> acc
          Just "templateparamlist"    -> acc
          Just "inbodydescription"    -> acc
          Just "scope"                -> acc
          Just "bitfield"             -> acc
          -- Unknown → warn
          Just other -> MemberParts
            { warnings = unknownChildWarning "memberdef" other : warnings, .. }
          Nothing -> acc

    classifyEnumValue :: [Cursor] -> EnumValueParts
    classifyEnumValue = foldr go (EnumValueParts [] Nothing [] [])
      where
        go c acc@EnumValueParts{..} = case nodeElementName c of
          Just "name"                 -> EnumValueParts { name = Just (extractText c), .. }
          Just "briefdescription"     -> EnumValueParts { briefs = c : briefs, .. }
          Just "detaileddescription"  -> EnumValueParts { details = c : details, .. }
          -- Known but ignored:
          Just "initializer"          -> acc
          Just "inbodydescription"    -> acc
          Just "location"             -> acc
          -- Unknown → warn
          Just other -> EnumValueParts
            { warnings = unknownChildWarning "enumvalue" other : warnings, .. }
          Nothing -> acc

-- | Classified children of a @\<compounddef\>@ element
data CompoundParts = CompoundParts {
    warnings     :: [Warning]
  , name         :: Maybe Text
  , title        :: Maybe Text
  , briefs       :: [Cursor]
  , details      :: [Cursor]
  , sections     :: [Cursor]
  , innerGroups  :: [Cursor]
  , innerClasses :: [Cursor]
  }

-- | Classified children of a @\<memberdef\>@ element
data MemberParts = MemberParts {
    warnings      :: [Warning]
  , name          :: Maybe Text
  , qualifiedName :: Maybe Text
  , briefs        :: [Cursor]
  , details       :: [Cursor]
  , enumValues    :: [Cursor]
  }

-- | Classified children of an @\<enumvalue\>@ element
data EnumValueParts = EnumValueParts {
    warnings :: [Warning]
  , name     :: Maybe Text
  , briefs   :: [Cursor]
  , details  :: [Cursor]
  }

-- | Intermediate result from extracting a single @\<memberdef\>@
data MemberInfo = MemberInfo {
    miName       :: Text
  , miComment    :: Comment Text
  , miQualName   :: Maybe Text   -- ^ e.g. @\"config_t::id\"@ for struct fields
  , miEnumValues :: [Cursor]     -- ^ @\<enumvalue\>@ children (enum memberdefs only)
  }

{-------------------------------------------------------------------------------
  Shared XML helpers
-------------------------------------------------------------------------------}

-- | Extract text content from an element's children
extractText :: Cursor -> Text
extractText c = Text.concat $ c $/ Cursor.content

-- | Get the local element name from a cursor, if it's an element node
nodeElementName :: Cursor -> Maybe Text
nodeElementName c = case Cursor.node c of
  XML.NodeElement el -> Just (XML.nameLocalName (XML.elementName el))
  _                  -> Nothing

-- | What to do with a child element
data ChildAction a
  = Skip      -- ^ Known child, not needed here — skip silently
  | Yield a   -- ^ Known child, include in results

-- | Process element children, dispatching on element name.
--
-- For each element child, calls the handler with the element name and
-- cursor.  The handler returns:
--
--   * @Just (Yield x)@ — include @x@ in the result list
--   * @Just Skip@ — known child, skip silently
--   * @Nothing@ — unknown child, emit a 'StructureLevel' warning
--
-- Text and other non-element nodes are silently skipped.
forChildren
  :: Text                                      -- ^ Parent element name (for warnings)
  -> [Cursor]                                  -- ^ Children to iterate
  -> (Text -> Cursor -> Maybe (ChildAction a)) -- ^ Handler
  -> ([Warning], [a])
forChildren parent cs handler = foldr go ([], []) cs
  where
    go c (ws, xs) = case nodeElementName c of
      Just n -> case handler n c of
        Just (Yield x) -> (ws, x : xs)
        Just Skip      -> (ws, xs)
        Nothing        -> (unknownChildWarning parent n : ws, xs)
      Nothing -> (ws, xs)

-- | Construct a 'StructureLevel' warning for an unknown child element
unknownChildWarning :: Text -> Text -> Warning
unknownChildWarning parent child = Warning {
    element     = child
  , context     = StructureLevel parent
  , degradation = Omitted
  , explanation = "unknown child <" <> child <> "> of <" <> parent <> ">; ignored"
  }

-- | Unsupported block-level element (preserved as 'Tag')
unsupportedBlockWarning :: Text -> Warning
unsupportedBlockWarning el = Warning {
    element     = el
  , context     = BlockLevel
  , degradation = Omitted
  , explanation = "unsupported block element: " <> el
  }

-- | Unsupported inline-level element (content preserved as plain text)
degradedInlineWarning :: Text -> Warning
degradedInlineWarning el = Warning {
    element     = el
  , context     = InlineLevel
  , degradation = DegradedToText
  , explanation = el <> " text preserved as plain text (no AST representation)"
  }

-- | Unknown @\<simplesect\>@ kind (defaulted to Note)
unknownSectKindWarning :: Text -> Warning
unknownSectKindWarning kind = Warning {
    element     = kind
  , context     = UnknownSectKind
  , degradation = DefaultedTo "Note"
  , explanation = "unknown simplesect kind '" <> kind <> "'; defaulted to Note"
  }

-- | Missing kind attribute on @\<simplesect\>@ (defaulted to Note)
missingSectKindWarning :: Warning
missingSectKindWarning = Warning {
    element     = "simplesect"
  , context     = UnknownSectKind
  , degradation = DefaultedTo "Note"
  , explanation = "simplesect with no kind attribute; defaulted to Note"
  }

-- | Collect warnings and results from a list of warning\/result pairs
collectWarnings :: [([a], [b])] -> ([a], [b])
collectWarnings pairs = (concatMap fst pairs, concatMap snd pairs)

-- | Extract brief and detailed descriptions, build a 'Comment'.
--
-- Takes the @\<briefdescription\>@ and @\<detaileddescription\>@ cursors
-- (already classified by the caller's fold).  Enumerates brief children
-- for @\<para\>@s, warning on unexpected elements.
extractBriefAndDetail :: [Cursor] -> [Cursor] -> ([Warning], Maybe (Comment Text))
extractBriefAndDetail briefDescs detailDescs =
  let (briefDescWarns, briefParas) =
        collectWarnings $ map classifyBriefChildren briefDescs
      detailChildren = concatMap Cursor.child detailDescs
      (commentWarns, mComment) = buildComment briefParas detailChildren
  in  (briefDescWarns ++ commentWarns, mComment)
  where
    classifyBriefChildren :: Cursor -> ([Warning], [Cursor])
    classifyBriefChildren bd =
      forChildren "briefdescription" (Cursor.child bd) $
        \n c -> case n of
          "para" -> Just (Yield c)
          _      -> Nothing

-- | Build a 'Comment' from pre-classified brief @\<para\>@s and detailed
-- children.
buildComment :: [Cursor] -> [Cursor] -> ([Warning], Maybe (Comment Text))
buildComment briefParas detailChildren =
  let (briefWarns, briefInlines) =
        collectWarnings $ map parseInlineChildren briefParas
      trimmedBrief = trimEdges briefInlines
      (detailWarns, detailedBlocks) =
        collectWarnings $ map parseBlockElement detailChildren
  in  case (trimmedBrief, detailedBlocks) of
        ([], []) -> (briefWarns ++ detailWarns, Nothing)
        _        -> ( briefWarns ++ detailWarns
                    , Just Comment {
                        brief    = trimmedBrief
                      , detailed = detailedBlocks
                      })

{-------------------------------------------------------------------------------
  Block and inline parsing
-------------------------------------------------------------------------------}

-- | Parse block-level content from a cursor
parseBlockElement :: Cursor -> ([Warning], [Block Text])
parseBlockElement cursor =
  case Cursor.node cursor of
    XML.NodeElement el -> parseBlock el cursor
    _                  -> ([], [])

-- | Parse a block-level XML element
parseBlock :: XML.Element -> Cursor -> ([Warning], [Block Text])
parseBlock el cursor = case XML.nameLocalName (XML.elementName el) of
  "para" ->
    let children = Cursor.child cursor
        (warns, blocks, inlines) = partitionContent children
        trimmed = trimEdges inlines
    in  (warns,
         [Paragraph trimmed | not (null trimmed)]
         ++ blocks)

  "parameterlist" ->
    let kind = case listToMaybe $ Cursor.attribute "kind" cursor of
                 Just "retval" -> ParamListRetVal
                 _             -> ParamListParam
        (childWarns, items) = forChildren "parameterlist" (Cursor.child cursor) $ \n c ->
          case n of
            "parameteritem" -> Just (Yield c)
            _               -> Nothing
        (warns, params) = parseParamItems items
    in  (childWarns ++ warns, [ParamList kind params])

  "simplesect" ->
    let children = Cursor.child cursor
        titles = [extractText c | c <- children, nodeElementName c == Just "title"]
        (kindWarns, kind) = parseSimpleSectKind cursor (listToMaybe titles)
        -- Filter <title> — already consumed by parseSimpleSectKind for SSPar
        contentChildren = filter (not . isTitle) children
        (contentWarns, content) = unzipBlocks contentChildren
    in  (kindWarns ++ contentWarns, [SimpleSect kind content])

  "programlisting" ->
    let (childWarns, codelines) = forChildren "programlisting" (Cursor.child cursor) $ \n c ->
          case n of
            "codeline" -> Just (Yield c)
            _          -> Nothing
        (codeWarns, codeLines) = unzip $ map extractCodeLine codelines
    in  (childWarns ++ concat codeWarns, [CodeBlock codeLines])

  "itemizedlist" ->
    let (childWarns, items) = forChildren "itemizedlist" (Cursor.child cursor) $ \n c ->
          case n of
            "listitem" -> Just (Yield c)
            _          -> Nothing
        (warns, blocks) = unzipListItems items
    in  (childWarns ++ warns, [ItemizedList blocks])

  "orderedlist" ->
    let (childWarns, items) = forChildren "orderedlist" (Cursor.child cursor) $ \n c ->
          case n of
            "listitem" -> Just (Yield c)
            _          -> Nothing
        (warns, blocks) = unzipListItems items
    in  (childWarns ++ warns, [OrderedList blocks])

  "xrefsect" ->
    let (childWarns, parts) = forChildren "xrefsect" (Cursor.child cursor) $ \n c ->
          case n of
            "xreftitle"       -> Just (Yield (Left (Text.strip (extractText c))))
            "xrefdescription" -> Just (Yield (Right (Cursor.child c)))
            _                 -> Nothing
        title = fromMaybe "" $ listToMaybe [t | Left t <- parts]
        (descWarns, desc) = unzipBlocks (concat [cs | Right cs <- parts])
    in  (childWarns ++ descWarns, [XRefSect title desc])

  "table" ->
    let (childWarns, children) = unzipBlocks (Cursor.child cursor)
    in  (childWarns, [Tag "table" children])

  other ->
    let (childWarns, children) = unzipBlocks (Cursor.child cursor)
    in  (unsupportedBlockWarning other : childWarns, [Tag other children])

-- | Partition children of a @\<para\>@ into block and inline content
--
partitionContent :: [Cursor] -> ([Warning], [Block Text], [Inline Text])
partitionContent = go [] [] []
  where
    go warns blocks inlines [] = (reverse warns, reverse blocks, reverse inlines)
    go warns blocks inlines (c : rest) =
      case Cursor.node c of
        XML.NodeContent txt
          | Text.all Char.isSpace txt ->
              go warns blocks (Text " " : inlines) rest
          | otherwise ->
              go warns blocks (Text (normalizeWhitespace txt) : inlines) rest
        XML.NodeElement el
          | XML.nameLocalName (XML.elementName el) `elem` blockElements
          -> let (bw, bs) = parseBlock el c
             in  go (bw ++ warns) (reverse bs ++ blocks) inlines rest
          | otherwise
          -> let (iw, is) = parseInline el c
             in  go (iw ++ warns) blocks (reverse is ++ inlines) rest
        _ -> go warns blocks inlines rest

    blockElements :: [Text]
    blockElements =
      [ "parameterlist", "simplesect", "programlisting"
      , "itemizedlist", "orderedlist", "xrefsect"
      , "table"
      ]

-- | Parse inline content from an XML element
parseInline :: XML.Element -> Cursor -> ([Warning], [Inline Text])
parseInline el cursor = case XML.nameLocalName (XML.elementName el) of
  "bold"           -> let (w, is) = parseInlineChildren cursor in (w, [Bold is])
  "emphasis"       -> let (w, is) = parseInlineChildren cursor in (w, [Emph is])
  "computeroutput" -> let (w, is) = parseInlineChildren cursor in (w, [Mono is])
  "ref"            -> let t = Text.strip $ Text.concat $ cursor $/ Cursor.content
                      in  ([], [Ref t t])
  "anchor"         -> ([], [Anchor $ Text.concat $ Cursor.attribute "id" cursor])
  "ulink"          -> let (w, is) = parseInlineChildren cursor
                      in  (w, [Link is (Text.concat $ Cursor.attribute "url" cursor)])
  "linebreak"      -> ([], [Text "\n"])
  "sp"             -> ([], [Text " "])
  other ->
    let txt = Text.strip $ Text.concat $ cursor $// Cursor.content
        w   = degradedInlineWarning other
    in  if Text.null txt
          then ([w], [])
          else ([w], [Text txt])

-- | Parse all inline children of a cursor
parseInlineChildren :: Cursor -> ([Warning], [Inline Text])
parseInlineChildren cursor = collectWarnings $ map go (Cursor.child cursor)
  where
    go :: Cursor -> ([Warning], [Inline Text])
    go c = case Cursor.node c of
      XML.NodeContent txt
        | Text.all Char.isSpace txt -> ([], [Text " "])
        | otherwise -> ([], [Text (normalizeWhitespace txt)])
      XML.NodeElement el -> parseInline el c
      _                  -> ([], [])

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Normalize whitespace in an XML text node
--
-- Collapses runs of whitespace to a single space, preserving a leading
-- and\/or trailing space when the original text started\/ended with
-- whitespace.  This is important for mixed-content XML: the text node
-- @\"Hello \"@ in @\<para\>Hello \<bold\>world\<\/bold\>\<\/para\>@ must
-- keep its trailing space so that the rendered output reads
-- @\"Hello world\"@ rather than @\"Helloworld\"@.
--
normalizeWhitespace :: Text -> Text
normalizeWhitespace txt
    | Text.null txt = txt
    | otherwise =
        let leading  = if Char.isSpace (Text.head txt) then " " else ""
            trailing = if Char.isSpace (Text.last txt) then " " else ""
            inner    = Text.unwords $ Text.words txt
        in  leading <> inner <> trailing

-- | Trim leading whitespace from the first 'Text' and trailing
-- whitespace from the last 'Text' in an inline sequence.
--
-- This is applied at paragraph boundaries (when constructing
-- 'Paragraph' and the brief description) to remove XML formatting
-- whitespace from paragraph edges while preserving inter-element
-- whitespace within the paragraph.
--
trimEdges :: [Inline ref] -> [Inline ref]
trimEdges = trimTrailing . trimLeading
  where
    trimLeading :: [Inline ref] -> [Inline ref]
    trimLeading (Text t : rest)
      | Text.null stripped = trimLeading rest
      | otherwise          = Text stripped : rest
      where stripped = Text.stripStart t
    trimLeading xs = xs

    trimTrailing :: [Inline ref] -> [Inline ref]
    trimTrailing [] = []
    trimTrailing [Text t]
      | Text.null stripped = []
      | otherwise          = [Text stripped]
      where stripped = Text.stripEnd t
    trimTrailing (x : xs) = x : trimTrailing xs

parseParamItems :: [Cursor] -> ([Warning], [Param Text])
parseParamItems cursors =
    let pairs = map parseParamItem cursors
    in  (concatMap fst pairs, mapMaybe snd pairs)

parseParamItem :: Cursor -> ([Warning], Maybe (Param Text))
parseParamItem cursor =
  let (itemWarns, parts) =
        forChildren "parameteritem" (Cursor.child cursor) $
          \n c -> case n of
            "parameternamelist"    -> Just (Yield (Left c))
            "parameterdescription" -> Just (Yield (Right (Cursor.child c)))
            _                     -> Nothing

      nameListCursors = [c | Left c  <- parts]
      descChildren    = concat [cs | Right cs <- parts]

      (nameListWarns, nameElems) =
        collectWarnings $ map classifyParamNameList nameListCursors

  in case listToMaybe $ map extractText nameElems of
    Nothing    -> (itemWarns ++ nameListWarns, Nothing)
    Just pname ->
      let direction = listToMaybe nameElems >>= \n ->
            case listToMaybe $ Cursor.attribute "direction" n of
              Just "in"    -> Just DirIn
              Just "out"   -> Just DirOut
              Just "inout" -> Just DirInOut
              _            -> Nothing
          (descWarns, desc) = unzipBlocks descChildren
      in  (itemWarns ++ nameListWarns ++ descWarns,
           Just Param { paramName = pname, paramDirection = direction, paramDesc = desc })
  where
    classifyParamNameList :: Cursor -> ([Warning], [Cursor])
    classifyParamNameList nl =
      forChildren "parameternamelist" (Cursor.child nl) $
        \n c -> case n of
          "parametername" -> Just (Yield c)
          _               -> Nothing

parseSimpleSectKind :: Cursor -> Maybe Text -> ([Warning], SimpleSectKind)
parseSimpleSectKind cursor mTitle =
  case listToMaybe $ Cursor.attribute "kind" cursor of
    Just "return"     -> ([], SSReturn)
    Just "warning"    -> ([], SSWarning)
    Just "note"       -> ([], SSNote)
    Just "see"        -> ([], SSSee)
    Just "since"      -> ([], SSSince)
    Just "version"    -> ([], SSVersion)
    Just "pre"        -> ([], SSPre)
    Just "post"       -> ([], SSPost)
    Just "deprecated" -> ([], SSDeprecated)
    Just "remark"     -> ([], SSRemark)
    Just "attention"  -> ([], SSAttention)
    Just "todo"       -> ([], SSTodo)
    Just "invariant"  -> ([], SSInvariant)
    Just "author"     -> ([], SSAuthor)
    Just "date"       -> ([], SSDate)
    Just "par"        -> ([], SSPar $ maybe "" Text.strip mTitle)
    Just other -> ([unknownSectKindWarning other], SSNote)
    Nothing    -> ([missingSectKindWarning], SSNote)

extractCodeLine :: Cursor -> ([Warning], Text)
extractCodeLine cursor =
  let (childWarns, highlights) =
        forChildren "codeline" (Cursor.child cursor) $
          \n c -> case n of
            "highlight" -> Just (Yield c)
            _           -> Nothing
  in  (childWarns, Text.concat $ map extractHighlight highlights)
  where
    extractHighlight :: Cursor -> Text
    extractHighlight c = Text.concat $ map nodeText (Cursor.child c)

    nodeText :: Cursor -> Text
    nodeText c = case Cursor.node c of
      XML.NodeContent txt -> txt
      XML.NodeElement el -> case XML.nameLocalName (XML.elementName el) of
        "sp"  -> " "
        "ref" -> Text.concat $ c $/ Cursor.content
        _     -> Text.concat $ c $// Cursor.content
      _ -> ""

-- | Collect block elements from children, threading warnings
unzipBlocks :: [Cursor] -> ([Warning], [Block Text])
unzipBlocks = collectWarnings . map parseBlockElement

-- | Collect list items, threading warnings
unzipListItems :: [Cursor] -> ([Warning], [[Block Text]])
unzipListItems items =
    let pairs = map parseListItem items
    in  (concatMap fst pairs, map snd pairs)

parseListItem :: Cursor -> ([Warning], [Block Text])
parseListItem cursor = unzipBlocks (Cursor.child cursor)

-- | Check if a cursor points at a @\<title\>@ element
isTitle :: Cursor -> Bool
isTitle c = case Cursor.node c of
    XML.NodeElement el -> XML.nameLocalName (XML.elementName el) == "title"
    _                  -> False

-- | Check if a @\<memberdef\>@ is a struct\/union field.
--
-- Struct fields have a @\<qualifiedname\>@ child like @\"config_t::id\"@,
-- whereas top-level declarations do not.
isStructField :: Maybe Text -> Bool
isStructField (Just qn) = "::" `Text.isInfixOf` qn
isStructField Nothing   = False

-- | Split a qualified name like @\"config_t::id\"@ into @(\"config_t\", \"id\")@.
splitQualifiedName :: Text -> Maybe (Text, Text)
splitQualifiedName qualName =
    case Text.breakOn "::" qualName of
      (sname, rest)
        | Just fn <- Text.stripPrefix "::" rest
        , not (Text.null sname)
        , not (Text.null fn)
        -> Just (sname, fn)
      _ -> Nothing

-- | Filter for entity XML files (groups, structs, unions, files).
--
-- This is a deny-list: it excludes known non-entity files.  If future doxygen
-- versions add new auxiliary XML files, they would pass through and be parsed
-- as entities — harmlessly yielding an empty result since they won't contain
-- a recognised @\<compounddef\>@.
--
-- Excludes schema files (@.xsd@), XSLT files, @index.xml@, @Doxyfile.xml@,
-- @dir_*.xml@ (directory compounds), @namespace*.xml@ and @class*.xml@
-- (C++-only compounds), @deprecated.xml@, and @indexpage.xml@.
isEntityXML :: FilePath -> Bool
isEntityXML f =
       takeExtension f == ".xml"
    && f /= "index.xml"
    && not ("Doxyfile"  `List.isPrefixOf` f)
    && not ("dir_"      `List.isPrefixOf` f)
    && not ("namespace" `List.isPrefixOf` f)
    && not ("class"     `List.isPrefixOf` f)
    && f /= "deprecated.xml"
    && f /= "indexpage.xml"

-- | Read an XML file, wrapping parse errors in 'DoxygenXMLParseError'
readXML :: FilePath -> IO XML.Document
readXML path =
    XML.readFile XML.def path
      `catch` \(e :: SomeException) ->
        throwIO $ DoxygenXMLParseError path (show e)
