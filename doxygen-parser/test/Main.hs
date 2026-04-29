{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Test.QuickCheck ((===))
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Text.XML qualified as XML
import Text.XML.Cursor (Cursor)
import Text.XML.Cursor qualified as Cursor

import Doxygen.Parser
import Doxygen.Parser.Internal (ChildAction (..), XMLFileResult (..),
                                extractBriefAndDetail, extractEntity,
                                forChildren, normalizeWhitespace,
                                parseBlockElement, parseInlineChildren)

main :: IO ()
main =
    defaultMain $ testGroup "doxygen-parser"
      [ testGroup "normalizeWhitespace"    testNormalizeWhitespace
      , testGroup "inline parsing"         testInlineParsing
      , testGroup "inline nesting"         testInlineNesting
      , testGroup "whitespace handling"    testWhitespace
      , testGroup "block parsing"          testBlockParsing
      , testGroup "comment parsing"        testCommentParsing
      , testGroup "parameter parsing"      testParamParsing
      , testGroup "simplesect kinds"       testSimpleSectKinds
      , testGroup "list parsing"           testListParsing
      , testGroup "code block parsing"     testCodeBlockParsing
      , testGroup "structural warnings"    testStructuralWarnings
      , testGroup "XMLFileResult assembly" testXMLFileResult
      , testGroup "properties"             testProperties
      ]

{-------------------------------------------------------------------------------
  Test helpers: XML parsing
-------------------------------------------------------------------------------}

-- | Parse a raw XML string into a cursor pointing at the root element
mkCursor :: Text -> Cursor
mkCursor xml =
    Cursor.fromDocument $ XML.parseText_ XML.def (LText.fromStrict xml)

-- | Parse all children of @\<root\>@ as block elements
parseBlockFromXML :: Text -> ([Warning], [Block DoxyRef])
parseBlockFromXML xml =
    let root = mkCursor xml
        pairs = map parseBlockElement (Cursor.child root)
    in  (concatMap fst pairs, concatMap snd pairs)

-- | Parse comment from an XML element with brief/detailed descriptions
parseCommentFromXML :: Text -> ([Warning], Maybe (Comment DoxyRef))
parseCommentFromXML xml =
  let cursor = mkCursor xml
      (_warns, parts) = forChildren "root" (Cursor.child cursor) $ \n c ->
        case n of
          "briefdescription"    -> Just (Yield (Left c))
          "detaileddescription" -> Just (Yield (Right c))
          _                     -> Just Skip
      briefDescs = [c | Left c <- parts]
      detailDescs = [c | Right c <- parts]
  in  extractBriefAndDetail briefDescs detailDescs

-- | Parse inline children of the root element
parseInlinesFromXML :: Text -> ([Warning], [Inline DoxyRef])
parseInlinesFromXML xml = parseInlineChildren (mkCursor xml)

-- | Wrap content in a root element
wrap :: Text -> Text
wrap content = "<root>" <> content <> "</root>"

{-------------------------------------------------------------------------------
  Test helpers: assertion combinators
-------------------------------------------------------------------------------}

-- | Assert that parsing an inline XML fragment produces the expected inlines
-- with no warnings.
inlineShouldBe :: Text -> [Inline DoxyRef] -> Assertion
inlineShouldBe xml expected = do
    let (ws, is) = parseInlinesFromXML (wrap xml)
    ws @?= []
    is @?= expected

-- | Assert that parsing a block XML fragment produces a single block matching
-- the predicate, with no warnings.
blockShouldMatch :: Text -> (Block DoxyRef -> Assertion) -> Assertion
blockShouldMatch xml check = do
    let (ws, bs) = parseBlockFromXML (wrap xml)
    ws @?= []
    case bs of
      [b] -> check b
      _   -> assertFailure $ "expected exactly 1 block, got: " ++ show bs

-- | Assert that warnings contain exactly one warning with the given structural
-- context and element name.
shouldWarnAbout :: [Warning] -> Text -> Text -> Assertion
shouldWarnAbout ws parentCtx elemName = do
    let filtered = filter (\w -> w.context == StructureLevel parentCtx) ws
    case filtered of
      [w] -> w.element @?= elemName
      _   -> assertFailure $
               "expected 1 " ++ Text.unpack parentCtx
               ++ " warning about " ++ Text.unpack elemName
               ++ ", got: " ++ show filtered

{-------------------------------------------------------------------------------
  Test helpers: XML builders for parameterlist
-------------------------------------------------------------------------------}

-- | Build a @\<parameterlist\>@ XML fragment.
mkParamListXML :: Text -> [(Maybe Text, Text, Text)] -> Text
mkParamListXML kind params = Text.concat $
    ["<parameterlist kind=\"", kind, "\">"]
    ++ concatMap mkItem params
    ++ ["</parameterlist>"]
  where
    mkItem (mDir, name, desc) =
      [ Text.concat
          [ "<parameteritem>"
          , "  <parameternamelist>"
          , "    <parametername", dirAttr mDir, ">", name, "</parametername>"
          , "  </parameternamelist>"
          , "  <parameterdescription><para>", desc, "</para></parameterdescription>"
          , "</parameteritem>"
          ]
      ]

    dirAttr Nothing    = ""
    dirAttr (Just dir) = " direction=\"" <> dir <> "\""

-- | Build a comment XML element with brief and detailed descriptions.
mkCommentXML :: Text -> Text -> Text
mkCommentXML brief detailed = Text.concat
    [ "<root>"
    , "  <briefdescription>", wrapPara brief, "</briefdescription>"
    , "  <detaileddescription>", wrapPara detailed, "</detaileddescription>"
    , "</root>"
    ]
  where
    wrapPara "" = ""
    wrapPara t  = "<para>" <> t <> "</para>"

{-------------------------------------------------------------------------------
  Test helpers: XML builders for entity extraction
-------------------------------------------------------------------------------}

-- | Wrap compound definitions in a @\<doxygen\>@ root.
mkDoxygen :: Text -> Text
mkDoxygen body = "<doxygen>" <> body <> "</doxygen>"

-- | Build a @\<compounddef\>@ with brief description and body content.
mkCompound :: Text -> Text -> Text -> Text -> Text -> Text
mkCompound kind cid name brief body = Text.concat
    [ "<compounddef kind=\"", kind, "\" id=\"", cid, "\">"
    , "<compoundname>", name, "</compoundname>"
    , "<briefdescription>", wrapPara brief, "</briefdescription>"
    , "<detaileddescription></detaileddescription>"
    , body
    , "</compounddef>"
    ]
  where
    wrapPara "" = ""
    wrapPara t  = "<para>" <> t <> "</para>"

-- | Build a @\<sectiondef\>@.
mkSection :: Text -> Text -> Text
mkSection kind body = Text.concat
    [ "<sectiondef kind=\"", kind, "\">"
    , body
    , "</sectiondef>"
    ]

-- | Build a @\<memberdef\>@ with brief description and optional body content.
mkMember :: Text -> Text -> Text -> Text -> Text -> Text
mkMember kind mid name brief body = Text.concat
    [ "<memberdef kind=\"", kind, "\" id=\"", mid, "\">"
    , "<name>", name, "</name>"
    , "<briefdescription>", wrapPara brief, "</briefdescription>"
    , "<detaileddescription></detaileddescription>"
    , body
    , "</memberdef>"
    ]
  where
    wrapPara "" = ""
    wrapPara t  = "<para>" <> t <> "</para>"

-- | Build an @\<enumvalue\>@.
mkEnumVal :: Text -> Text -> Text -> Text -> Text
mkEnumVal evid name brief body = Text.concat
    [ "<enumvalue id=\"", evid, "\">"
    , "<name>", name, "</name>"
    , "<briefdescription><para>", brief, "</para></briefdescription>"
    , "<detaileddescription></detaileddescription>"
    , body
    , "</enumvalue>"
    ]

-- | Parse a @\<compounddef\>@ from XML and run assertions on the result.
withExtractedEntity :: Text -> (XMLFileResult -> Assertion) -> Assertion
withExtractedEntity xml check = do
    let doc = XML.parseText_ XML.def (LText.fromStrict xml)
        root = Cursor.fromDocument doc
    let (_warns, cds) = forChildren "doxygen" (Cursor.child root) $ \n c ->
          case n of
            "compounddef" -> Just (Yield c)
            _             -> Just Skip
    case cds of
      []     -> assertFailure "no compounddef found"
      (cd:_) -> check (extractEntity cd)

{-------------------------------------------------------------------------------
  normalizeWhitespace
-------------------------------------------------------------------------------}

testNormalizeWhitespace :: [TestTree]
testNormalizeWhitespace =
  [ testCase "plain text"                     $ normalizeWhitespace "hello"          @?= "hello"
  , testCase "preserves trailing space"       $ normalizeWhitespace "hello "         @?= "hello "
  , testCase "preserves leading space"        $ normalizeWhitespace " hello"         @?= " hello"
  , testCase "preserves both"                 $ normalizeWhitespace " hello "        @?= " hello "
  , testCase "collapses internal whitespace"  $ normalizeWhitespace "hello   world"  @?= "hello world"
  , testCase "collapses newlines"             $ normalizeWhitespace "hello\n  world" @?= "hello world"
  , testCase "leading newline becomes space"  $ normalizeWhitespace "\n  hello"      @?= " hello"
  , testCase "trailing newline becomes space" $ normalizeWhitespace "hello\n"        @?= "hello "
  ]

{-------------------------------------------------------------------------------
  Inline parsing
-------------------------------------------------------------------------------}

testInlineParsing :: [TestTree]
testInlineParsing =
  [ testCase "plain text" $
      "hello" `inlineShouldBe` [Text "hello"]
  , testCase "bold" $
      "<bold>text</bold>" `inlineShouldBe` [Bold [Text "text"]]
  , testCase "emphasis" $
      "<emphasis>text</emphasis>" `inlineShouldBe` [Emph [Text "text"]]
  , testCase "computeroutput" $
      "<computeroutput>code</computeroutput>" `inlineShouldBe` [Mono [Text "code"]]
  , testCase "ref without kindref" $
      "<ref refid=\"abc\">my_func</ref>" `inlineShouldBe` [Ref (DoxyRef "my_func" Nothing) "my_func"]
  , testCase "ref with kindref compound" $
      "<ref refid=\"structfoo\" kindref=\"compound\">foo</ref>"
        `inlineShouldBe` [Ref (DoxyRef "foo" (Just RefCompound)) "foo"]
  , testCase "ref with kindref member" $
      "<ref refid=\"file_1abc\" kindref=\"member\">bar</ref>"
        `inlineShouldBe` [Ref (DoxyRef "bar" (Just RefMember)) "bar"]
  , testCase "anchor" $
      "<anchor id=\"foo\"/>" `inlineShouldBe` [Anchor "foo"]
  , testCase "ulink" $
      "<ulink url=\"http://example.com\">click</ulink>"
        `inlineShouldBe` [Link [Text "click"] "http://example.com"]
  , testCase "linebreak" $
      "<linebreak/>" `inlineShouldBe` [Text "\n"]
  , testCase "sp" $
      "<sp/>" `inlineShouldBe` [Text " "]
  , testCase "empty bold" $
      "<bold></bold>" `inlineShouldBe` [Bold []]
  , testCase "empty emphasis" $
      "<emphasis></emphasis>" `inlineShouldBe` [Emph []]

  , testCase "special XML characters" $
      "a &lt; b &amp; c &gt; d" `inlineShouldBe` [Text "a < b & c > d"]

  , testCase "unknown inline degrades to text" $ do
      let (ws, is) = parseInlinesFromXML (wrap "<subscript>x</subscript>")
      case ws of
        [w] -> do
          w.context @?= InlineLevel
          w.degradation @?= DegradedToText
        _ -> assertFailure $ "expected 1 warning, got: " ++ show ws
      is @?= [Text "x"]

  , testCase "unknown inline with empty text" $ do
      let (ws, is) = parseInlinesFromXML (wrap "<subscript/>")
      length ws @?= 1
      is @?= []
  ]

{-------------------------------------------------------------------------------
  Inline nesting
-------------------------------------------------------------------------------}

testInlineNesting :: [TestTree]
testInlineNesting =
  [ testCase "bold inside emphasis" $
      "<emphasis><bold>text</bold></emphasis>"
        `inlineShouldBe` [Emph [Bold [Text "text"]]]
  , testCase "code inside bold" $
      "<bold><computeroutput>code</computeroutput></bold>"
        `inlineShouldBe` [Bold [Mono [Text "code"]]]
  , testCase "3-deep nesting: bold > emphasis > mono" $
      "<bold><emphasis><computeroutput>deep</computeroutput></emphasis></bold>"
        `inlineShouldBe` [Bold [Emph [Mono [Text "deep"]]]]
  , testCase "nested emphasis" $
      "<emphasis>outer <emphasis>inner</emphasis> outer</emphasis>"
        `inlineShouldBe`
          [Emph [Text "outer ", Emph [Text "inner"], Text " outer"]]
  , testCase "ref inside emphasis" $
      "<emphasis><ref refid=\"x\">name</ref></emphasis>"
        `inlineShouldBe` [Emph [Ref (DoxyRef "name" Nothing) "name"]]
  , testCase "link inside bold" $
      "<bold><ulink url=\"http://x\">text</ulink></bold>"
        `inlineShouldBe` [Bold [Link [Text "text"] "http://x"]]
  , testCase "mixed siblings: text + bold + text + emphasis" $
      "Hello <bold>world</bold> and <emphasis>more</emphasis>!"
        `inlineShouldBe`
          [ Text "Hello ", Bold [Text "world"]
          , Text " and ", Emph [Text "more"]
          , Text "!"
          ]
  ]

{-------------------------------------------------------------------------------
  Whitespace handling
-------------------------------------------------------------------------------}

testWhitespace :: [TestTree]
testWhitespace =
  [ testCase "space between text and bold is preserved" $
      "Hello <bold>world</bold>"
        `inlineShouldBe` [Text "Hello ", Bold [Text "world"]]

  , testCase "space after bold is preserved" $
      "<bold>word</bold> rest"
        `inlineShouldBe` [Bold [Text "word"], Text " rest"]

  , testCase "inter-element spacing in sentence" $
      "Use <computeroutput>foo</computeroutput> for bar"
        `inlineShouldBe` [Text "Use ", Mono [Text "foo"], Text " for bar"]

  , testCase "whitespace-only text node is normalized to space" $
      "   " `inlineShouldBe` [Text " "]
  , testCase "newline-only text is normalized to space" $
      "\n  \n" `inlineShouldBe` [Text " "]
  , testCase "unicode content preserved" $
      "caf\233 na\239ve" `inlineShouldBe` [Text "caf\233 na\239ve"]

  , testCase "whitespace between sibling inline elements in para" $ do
      let (ws, bs) = parseBlockFromXML
            (wrap "<para><bold>a</bold> <emphasis>b</emphasis></para>")
      ws @?= []
      case bs of
        [Paragraph inlines] ->
          inlines @?= [Bold [Text "a"], Text " ", Emph [Text "b"]]
        _ -> assertFailure $ "unexpected: " ++ show bs
  ]

{-------------------------------------------------------------------------------
  Block parsing
-------------------------------------------------------------------------------}

testBlockParsing :: [TestTree]
testBlockParsing =
  [ testCase "paragraph" $
      blockShouldMatch "<para>Hello</para>" $ \b ->
        b @?= Paragraph [Text "Hello"]

  , testCase "empty paragraph produces nothing" $ do
      let (ws, bs) = parseBlockFromXML (wrap "<para></para>")
      ws @?= []
      bs @?= []

  , testCase "whitespace-only paragraph produces nothing" $ do
      let (ws, bs) = parseBlockFromXML (wrap "<para>   </para>")
      ws @?= []
      bs @?= []

  , testCase "para with mixed block and inline content" $ do
      let (_, bs) = parseBlockFromXML $ wrap $ Text.concat
            [ "<para>"
            , "Some text"
            , "<simplesect kind=\"note\"><para>A note</para></simplesect>"
            , "</para>"
            ]
      bs @?= [ Paragraph [Text "Some text"]
             , SimpleSect SSNote [Paragraph [Text "A note"]]
             ]

  , testCase "para with only a parameterlist produces no empty paragraph" $ do
      let (_, bs) = parseBlockFromXML $ wrap $ Text.concat
            [ "<para>"
            , mkParamListXML "param" [(Nothing, "x", "Desc")]
            , "</para>"
            ]
      bs @?= [ParamList ParamListParam
                [Param { paramName = "x"
                       , paramDirection = Nothing
                       , paramDesc = [Paragraph [Text "Desc"]]
                       }]]

  , testCase "parameterlist (param kind)" $
      blockShouldMatch (mkParamListXML "param" [(Just "in", "x", "The input")]) $
        \case
          ParamList ParamListParam [p] -> do
            p.paramName @?= "x"
            p.paramDirection @?= Just DirIn
          b -> assertFailure $ "unexpected: " ++ show b

  , testCase "parameterlist (retval kind)" $
      blockShouldMatch (mkParamListXML "retval" [(Nothing, "0", "Success")]) $
        \case
          ParamList ParamListRetVal _ -> pure ()
          b -> assertFailure $ "expected retval list, got: " ++ show b

  , testCase "simplesect return" $
      blockShouldMatch "<simplesect kind=\"return\"><para>The result</para></simplesect>" $
        \case
          SimpleSect SSReturn _ -> pure ()
          b -> assertFailure $ "expected SSReturn: " ++ show b

  , testCase "programlisting" $
      blockShouldMatch
        ("<programlisting>"
         <> "<codeline><highlight class=\"normal\">int<sp/>x<sp/>=<sp/>0;</highlight></codeline>"
         <> "</programlisting>") $
        \case
          CodeBlock [line] -> line @?= "int x = 0;"
          b -> assertFailure $ "expected code block: " ++ show b

  , testCase "xrefsect (deprecated)" $
      blockShouldMatch
        ("<xrefsect id=\"deprecated\">"
         <> "<xreftitle>Deprecated</xreftitle>"
         <> "<xrefdescription><para>Use new_func instead</para></xrefdescription>"
         <> "</xrefsect>") $
        \case
          XRefSect "Deprecated" _ -> pure ()
          b -> assertFailure $ "expected xrefsect: " ++ show b

  , testCase "table preserves content as Tag without warning" $ do
      let (ws, bs) = parseBlockFromXML (wrap "<table><row><entry><para>cell</para></entry></row></table>")
      assertBool ("no table warning expected, got: " ++ show [w | w <- ws, w.element == "table"])
                 (not $ any (\w -> w.element == "table") ws)
      case bs of
        [Tag tag _children] -> tag @?= "table"
        _ -> assertFailure $ "expected Tag: " ++ show bs

  , testCase "unknown block element emits warning + Tag" $ do
      let (ws, bs) = parseBlockFromXML (wrap "<sect1><title>Heading</title></sect1>")
      case ws of
        (w : _) -> w.context @?= BlockLevel
        _       -> assertFailure $ "expected at least 1 warning, got: " ++ show ws
      case bs of
        [Tag tag _children] -> tag @?= "sect1"
        _ -> assertFailure $ "expected Tag: " ++ show bs
  ]

{-------------------------------------------------------------------------------
  Comment parsing
-------------------------------------------------------------------------------}

testCommentParsing :: [TestTree]
testCommentParsing =
  [ testCase "brief and detailed" $ do
      let (ws, mc) = parseCommentFromXML (mkCommentXML "Brief text" "Detailed text")
      ws @?= []
      case mc of
        Just c -> do
          c.brief @?= [Text "Brief text"]
          length c.detailed @?= 1
        Nothing -> assertFailure "expected a comment"

  , testCase "brief only" $ do
      let (_, mc) = parseCommentFromXML (mkCommentXML "Brief only" "")
      case mc of
        Just c -> do
          c.brief @?= [Text "Brief only"]
          c.detailed @?= []
        Nothing -> assertFailure "expected a comment"

  , testCase "detailed only" $ do
      let (_, mc) = parseCommentFromXML (mkCommentXML "" "Detailed only")
      case mc of
        Just c -> do
          c.brief @?= []
          length c.detailed @?= 1
        Nothing -> assertFailure "expected a comment"

  , testCase "empty descriptions produce Nothing" $ do
      let (_, mc) = parseCommentFromXML (mkCommentXML "" "")
      mc @?= Nothing

  , testCase "brief with inline formatting" $ do
      let xml = Text.concat
            [ "<root>"
            , "  <briefdescription>"
            , "    <para>Use <computeroutput>foo</computeroutput> for bar</para>"
            , "  </briefdescription>"
            , "  <detaileddescription></detaileddescription>"
            , "</root>"
            ]
      let (_, mc) = parseCommentFromXML xml
      case mc of
        Just c ->
          assertBool "should have mono inline" $
            any (\case Mono _ -> True; _ -> False) c.brief
        Nothing -> assertFailure "expected a comment"

  , testCase "multiple paragraphs in detailed" $ do
      let (_, mc) = parseCommentFromXML $ Text.concat
            [ "<root>"
            , "  <briefdescription></briefdescription>"
            , "  <detaileddescription>"
            , "    <para>First paragraph</para>"
            , "    <para>Second paragraph</para>"
            , "  </detaileddescription>"
            , "</root>"
            ]
      case mc of
        Just c  -> length c.detailed @?= 2
        Nothing -> assertFailure "expected a comment"
  ]

{-------------------------------------------------------------------------------
  Parameter parsing
-------------------------------------------------------------------------------}

testParamParsing :: [TestTree]
testParamParsing =
  [ mkParamTest "param with direction=in"    (Just "in")    "x"      (Just DirIn)
  , mkParamTest "param with direction=out"   (Just "out")   "result" (Just DirOut)
  , mkParamTest "param with direction=inout" (Just "inout") "buf"    (Just DirInOut)
  , mkParamTest "param with no direction"    Nothing        "x"      Nothing

  , testCase "param with empty description" $
      blockShouldMatch (mkParamListXML "param" [(Nothing, "x", "")]) $
        \case
          -- Empty <para> in description is dropped, so paramDesc is empty
          -- (the mkParamListXML wraps desc in <para>, but empty text
          -- produces an empty paragraph which is dropped)
          ParamList _ [_p] -> pure ()
          b -> assertFailure $ "unexpected: " ++ show b

  , testCase "param with missing name is skipped" $ do
      let xml = wrap $ Text.concat
            [ "<parameterlist kind=\"param\">"
            , "<parameteritem>"
            , "  <parameternamelist></parameternamelist>"
            , "  <parameterdescription><para>Desc</para></parameterdescription>"
            , "</parameteritem>"
            , "</parameterlist>"
            ]
      let (_, bs) = parseBlockFromXML xml
      case bs of
        [ParamList _ params] ->
          length params @?= 0
        _ -> assertFailure $ "unexpected: " ++ show bs

  , testCase "multiple params" $
      blockShouldMatch
        (mkParamListXML "param"
          [ (Nothing, "a", "First")
          , (Nothing, "b", "Second")
          ]) $
        \case
          ParamList _ params -> length params @?= 2
          b -> assertFailure $ "unexpected: " ++ show b
  ]
  where
    mkParamTest :: String -> Maybe Text -> Text -> Maybe ParamDirection -> TestTree
    mkParamTest name mDir paramName expectedDir =
      testCase name $
        blockShouldMatch (mkParamListXML "param" [(mDir, paramName, "Desc")]) $
          \case
            ParamList _ [p] -> p.paramDirection @?= expectedDir
            b -> assertFailure $ "unexpected: " ++ show b

{-------------------------------------------------------------------------------
  SimpleSect kinds
-------------------------------------------------------------------------------}

testSimpleSectKinds :: [TestTree]
testSimpleSectKinds =
  [ mkSimpleSectTest "return"     SSReturn
  , mkSimpleSectTest "warning"    SSWarning
  , mkSimpleSectTest "note"       SSNote
  , mkSimpleSectTest "see"        SSSee
  , mkSimpleSectTest "since"      SSSince
  , mkSimpleSectTest "version"    SSVersion
  , mkSimpleSectTest "pre"        SSPre
  , mkSimpleSectTest "post"       SSPost
  , mkSimpleSectTest "deprecated" SSDeprecated
  , mkSimpleSectTest "remark"     SSRemark
  , mkSimpleSectTest "attention"  SSAttention
  , mkSimpleSectTest "todo"       SSTodo
  , mkSimpleSectTest "invariant"  SSInvariant
  , mkSimpleSectTest "author"     SSAuthor
  , mkSimpleSectTest "date"       SSDate

  , testCase "par with title" $
      blockShouldMatch
        ("<simplesect kind=\"par\">"
         <> "<title>My Title</title>"
         <> "<para>Content</para>"
         <> "</simplesect>") $
        \case
          SimpleSect (SSPar title) _ -> title @?= "My Title"
          b -> assertFailure $ "expected SSPar: " ++ show b

  , testCase "unknown kind defaults to SSNote with warning" $ do
      let (ws, bs) = parseBlockFromXML
            (wrap "<simplesect kind=\"bogus\"><para>X</para></simplesect>")
      case ws of
        [w] -> do
          w.context @?= UnknownSectKind
          w.degradation @?= DefaultedTo "Note"
        _ -> assertFailure $ "expected 1 warning, got: " ++ show ws
      case bs of
        [SimpleSect SSNote _] -> pure ()
        _ -> assertFailure $ "expected SSNote: " ++ show bs

  , testCase "missing kind defaults to SSNote with warning" $ do
      let (ws, bs) = parseBlockFromXML
            (wrap "<simplesect><para>X</para></simplesect>")
      case ws of
        [w] -> w.context @?= UnknownSectKind
        _   -> assertFailure $ "expected 1 warning, got: " ++ show ws
      case bs of
        [SimpleSect SSNote _] -> pure ()
        _ -> assertFailure $ "expected SSNote: " ++ show bs
  ]

mkSimpleSectTest :: Text -> SimpleSectKind -> TestTree
mkSimpleSectTest kindAttr expected =
  testCase (Text.unpack kindAttr) $
    blockShouldMatch
      ("<simplesect kind=\"" <> kindAttr <> "\"><para>Content</para></simplesect>") $
      \case
        SimpleSect k _ -> k @?= expected
        b -> assertFailure $ "expected simplesect: " ++ show b

{-------------------------------------------------------------------------------
  List parsing
-------------------------------------------------------------------------------}

testListParsing :: [TestTree]
testListParsing =
  [ testCase "itemized list" $
      blockShouldMatch
        ("<itemizedlist>"
         <> "<listitem><para>First</para></listitem>"
         <> "<listitem><para>Second</para></listitem>"
         <> "</itemizedlist>") $
        \case
          ItemizedList items -> length items @?= 2
          b -> assertFailure $ "expected itemized list: " ++ show b

  , testCase "ordered list" $
      blockShouldMatch
        ("<orderedlist>"
         <> "<listitem><para>First</para></listitem>"
         <> "<listitem><para>Second</para></listitem>"
         <> "<listitem><para>Third</para></listitem>"
         <> "</orderedlist>") $
        \case
          OrderedList items -> length items @?= 3
          b -> assertFailure $ "expected ordered list: " ++ show b

  , testCase "nested lists" $
      blockShouldMatch
        ("<itemizedlist>"
         <> "<listitem>"
         <> "  <para>Outer</para>"
         <> "  <itemizedlist>"
         <> "    <listitem><para>Inner</para></listitem>"
         <> "  </itemizedlist>"
         <> "</listitem>"
         <> "</itemizedlist>") $
        \case
          ItemizedList [item] ->
            assertBool "should have nested list" $
              any (\case ItemizedList _ -> True; _ -> False) item
          b -> assertFailure $ "expected nested list: " ++ show b

  , testCase "list item with formatted content" $
      blockShouldMatch
        ("<itemizedlist>"
         <> "<listitem><para>Use <bold>this</bold> function</para></listitem>"
         <> "</itemizedlist>") $
        \case
          ItemizedList [[Paragraph is]] ->
            assertBool "should have bold" $
              any (\case Bold _ -> True; _ -> False) is
          b -> assertFailure $ "unexpected: " ++ show b
  ]

{-------------------------------------------------------------------------------
  Code block parsing
-------------------------------------------------------------------------------}

testCodeBlockParsing :: [TestTree]
testCodeBlockParsing =
  [ testCase "single code line" $
      blockShouldMatch
        ("<programlisting>"
         <> "<codeline><highlight class=\"normal\">return<sp/>0;</highlight></codeline>"
         <> "</programlisting>") $
        \case
          CodeBlock [line] -> line @?= "return 0;"
          b -> assertFailure $ "expected code block: " ++ show b

  , testCase "multiple code lines" $
      blockShouldMatch
        ("<programlisting>"
         <> "<codeline><highlight class=\"normal\">int<sp/>x;</highlight></codeline>"
         <> "<codeline><highlight class=\"normal\">x<sp/>=<sp/>0;</highlight></codeline>"
         <> "</programlisting>") $
        \case
          CodeBlock codeLines -> length codeLines @?= 2
          b -> assertFailure $ "expected 2 code lines: " ++ show b

  , testCase "code line with ref" $
      blockShouldMatch
        ("<programlisting>"
         <> "<codeline><highlight class=\"normal\">"
         <> "<ref refid=\"abc\">my_type</ref><sp/>x;</highlight></codeline>"
         <> "</programlisting>") $
        \case
          CodeBlock [line] -> line @?= "my_type x;"
          b -> assertFailure $ "expected code block: " ++ show b

  , testCase "empty code block" $
      blockShouldMatch "<programlisting></programlisting>" $
        \case
          CodeBlock [] -> pure ()
          b -> assertFailure $ "expected empty code block: " ++ show b
  ]

{-------------------------------------------------------------------------------
  Structural warnings
-------------------------------------------------------------------------------}

testStructuralWarnings :: [TestTree]
testStructuralWarnings =
  [ testCase "unknown briefdescription child warns" $ do
      let xml = Text.concat
            [ "<root>"
            , "  <briefdescription>"
            , "    <para>Normal para</para>"
            , "    <bogus>unexpected</bogus>"
            , "  </briefdescription>"
            , "  <detaileddescription></detaileddescription>"
            , "</root>"
            ]
          (ws, mc) = parseCommentFromXML xml
      assertBool "should still produce a comment" $ mc /= Nothing
      shouldWarnAbout ws "briefdescription" "bogus"

  , testCase "unknown parameteritem child warns" $ do
      let (ws, _) = parseBlockFromXML $ wrap $ Text.concat
            [ "<parameterlist kind=\"param\">"
            , "  <parameteritem>"
            , "    <parameternamelist>"
            , "      <parametername direction=\"in\">x</parametername>"
            , "    </parameternamelist>"
            , "    <parameterdescription><para>The input.</para></parameterdescription>"
            , "    <alien/>"
            , "  </parameteritem>"
            , "</parameterlist>"
            ]
      shouldWarnAbout ws "parameteritem" "alien"

  , testCase "unknown programlisting child warns" $ do
      let (ws, bs) = parseBlockFromXML $ wrap $ Text.concat
            [ "<programlisting>"
            , "  <codeline><highlight class=\"normal\">code</highlight></codeline>"
            , "  <bogus/>"
            , "</programlisting>"
            ]
      shouldWarnAbout ws "programlisting" "bogus"
      case bs of
        [CodeBlock lines'] -> length lines' @?= 1
        _ -> assertFailure $ "expected CodeBlock: " ++ show bs

  , testCase "unknown codeline child warns" $ do
      let (ws, _) = parseBlockFromXML $ wrap $ Text.concat
            [ "<programlisting>"
            , "  <codeline>"
            , "    <highlight class=\"normal\">code</highlight>"
            , "    <mystery/>"
            , "  </codeline>"
            , "</programlisting>"
            ]
      shouldWarnAbout ws "codeline" "mystery"
  ]

{-------------------------------------------------------------------------------
  XMLFileResult assembly
-------------------------------------------------------------------------------}

testXMLFileResult :: [TestTree]
testXMLFileResult =
  [ testCase "struct compound extracts struct doc and field docs" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "struct" "structfoo" "foo_t" "A foo struct" $
            mkSection "public-attrib" $ Text.concat
                [ mkMember "variable" "field_x" "x" "X coordinate" ""
                , mkMember "variable" "field_y" "y" "Y coordinate" ""
                ]) $ \result -> do
          assertBool "should have struct doc" $
            Map.member (KeyStruct "foo_t") result.comments
          assertBool "should have field x" $
            Map.member (KeyField "foo_t" "x") result.comments
          assertBool "should have field y" $
            Map.member (KeyField "foo_t" "y") result.comments

  , testCase "group compound extracts title and member docs" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "group" "group__core" "core" "" $
            "<title>Core Functions</title>"
            <> mkSection "func"
                 (mkMember "function" "func_init" "init" "Initialize" "")
        ) $ \result -> do
          result.groupTitles @?= [("core", "Core Functions")]
          assertBool "should have init decl" $
            Map.member (KeyDecl "init") result.comments

  , testCase "enum member extracts enum value docs" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "file" "myfile_8h" "myfile.h" "" $
            mkSection "enum" $
              mkMember "enum" "enum_color" "color_t" "A color enum" $ Text.concat
                [ mkEnumVal "ev_red"  "RED"  "Red color"  ""
                , mkEnumVal "ev_blue" "BLUE" "Blue color" ""
                ]
        ) $ \result -> do
          assertBool "should have color_t decl" $
            Map.member (KeyDecl "color_t") result.comments
          assertBool "should have RED" $
            Map.member (KeyEnumValue "color_t" "RED") result.comments
          assertBool "should have BLUE" $
            Map.member (KeyEnumValue "color_t" "BLUE") result.comments

  , testCase "known-but-ignored compounddef children produce no warnings" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "struct" "structbar" "bar_t" "A bar struct" $
            "<location file=\"test.h\" line=\"1\"/>"
            <> "<includes refid=\"test_8h\">test.h</includes>"
        ) $ \result -> do
          assertBool "should have struct doc" $
            Map.member (KeyStruct "bar_t") result.comments
          result.warnings @?= []

  , testCase "truly unknown compounddef children emit warnings" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "struct" "structbar" "bar_t" "A bar struct"
            "<nonstandard>unexpected content</nonstandard>"
        ) $ \result -> do
          assertBool "should have struct doc" $
            Map.member (KeyStruct "bar_t") result.comments
          shouldWarnAbout result.warnings "compounddef" "nonstandard"
          case result.warnings of
            [w] -> w.degradation @?= Omitted
            _   -> assertFailure $ "expected 1 warning, got: " ++ show result.warnings

  , testCase "unknown memberdef children emit warnings" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "group" "group__test" "test" "" $
            mkSection "func" $
              mkMember "function" "test_1" "foo" "A function"
                "<type>void</type><bogus_tag>unexpected</bogus_tag>"
        ) $ \result -> do
          assertBool "should have decl doc" $
            Map.member (KeyDecl "foo") result.comments
          shouldWarnAbout result.warnings "memberdef" "bogus_tag"

  , testCase "unknown sectiondef children emit warnings" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "group" "group__test" "test" "" $
            mkSection "func" $
              mkMember "function" "test_1" "bar" "A function" ""
              <> "<unexpected_child/>"
        ) $ \result ->
          shouldWarnAbout result.warnings "sectiondef" "unexpected_child"

  , testCase "unknown enumvalue children emit warnings" $
      withExtractedEntity
        (mkDoxygen $ mkCompound "group" "group__test" "test" "" $
            mkSection "enum" $
              mkMember "enum" "test_1" "color_t" "Colors" $
                mkEnumVal "ev1" "RED" "Red"
                  "<initializer>= 0</initializer><alien_element/>"
        ) $ \result -> do
          assertBool "should have enum value doc" $
            Map.member (KeyEnumValue "color_t" "RED") result.comments
          shouldWarnAbout result.warnings "enumvalue" "alien_element"

  , testCase "compounddef with no compoundname produces empty result" $
      withExtractedEntity (Text.concat
        [ "<doxygen>"
        , "<compounddef kind=\"struct\" id=\"structempty\">"
        , "  <briefdescription><para>Orphaned doc</para></briefdescription>"
        , "  <detaileddescription></detaileddescription>"
        , "</compounddef>"
        , "</doxygen>"
        ]) $ \result ->
          result.comments @?= Map.empty
  ]

{-------------------------------------------------------------------------------
  Property tests
-------------------------------------------------------------------------------}

testProperties :: [TestTree]
testProperties =
  [ testProperty "normalizeWhitespace is idempotent" $ \(QC.PrintableString s) ->
      let t = Text.pack s
      in  normalizeWhitespace (normalizeWhitespace t) === normalizeWhitespace t

  , testProperty "normalizeWhitespace: no internal double spaces" $
      \(QC.PrintableString s) ->
        let t = Text.pack s
            result = normalizeWhitespace t
            -- Strip the (at most one) leading and trailing space
            interior = Text.dropWhileEnd (== ' ') $ Text.dropWhile (== ' ') result
        in  not (Text.isInfixOf "  " interior)
  ]
