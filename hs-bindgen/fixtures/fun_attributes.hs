[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "FILE",
      structConstr = HsName
        "@NsConstr"
        "FILE",
      structFields = [],
      structOrigin =
      Just
        Decl {
          declInfo =
          DeclInfo {
            declLoc =
            "fun_attributes.h:7:9",
            declId = NamePair {
              nameC = Name "FILE",
              nameHsIdent = HsIdentifier
                "FILE"},
            declOrigin = NameOriginGenerated
              (AnonId "fun_attributes.h:7:9"),
            declAliases = [Name "FILE"],
            declHeader = "fun_attributes.h",
            declComment =
            Just
              [
                Paragraph
                  [
                    TextContent
                      "Attributes on functions"],
                Paragraph
                  [
                    TextContent
                      "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "FILE"),
              structSizeof = 0,
              structAlignment = 1,
              structFields = [],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment =
      Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Attributes on functions"],
          commentOrigin = Just "FILE",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "FILE",
          structConstr = HsName
            "@NsConstr"
            "FILE",
          structFields = [],
          structOrigin =
          Just
            Decl {
              declInfo =
              DeclInfo {
                declLoc =
                "fun_attributes.h:7:9",
                declId = NamePair {
                  nameC = Name "FILE",
                  nameHsIdent = HsIdentifier
                    "FILE"},
                declOrigin = NameOriginGenerated
                  (AnonId "fun_attributes.h:7:9"),
                declAliases = [Name "FILE"],
                declHeader = "fun_attributes.h",
                declComment =
                Just
                  [
                    Paragraph
                      [
                        TextContent
                          "Attributes on functions"],
                    Paragraph
                      [
                        TextContent
                          "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "FILE"),
                  structSizeof = 0,
                  structAlignment = 1,
                  structFields = [],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment =
          Just
            Comment {
              commentTitle = Just
                [
                  TextContent
                    "Attributes on functions"],
              commentOrigin = Just "FILE",
              commentChildren =
              [
                Paragraph
                  [
                    TextContent
                      "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}}
        StorableInstance {
          storableSizeOf = 0,
          storableAlignment = 1,
          storablePeek =
          Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "FILE",
                  structConstr = HsName
                    "@NsConstr"
                    "FILE",
                  structFields = [],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "fun_attributes.h:7:9",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = HsIdentifier
                            "FILE"},
                        declOrigin = NameOriginGenerated
                          (AnonId "fun_attributes.h:7:9"),
                        declAliases = [Name "FILE"],
                        declHeader = "fun_attributes.h",
                        declComment =
                        Just
                          [
                            Paragraph
                              [
                                TextContent
                                  "Attributes on functions"],
                            Paragraph
                              [
                                TextContent
                                  "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "FILE"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [
                          TextContent
                            "Attributes on functions"],
                      commentOrigin = Just "FILE",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}})
              []),
          storablePoke =
          Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "FILE",
                  structConstr = HsName
                    "@NsConstr"
                    "FILE",
                  structFields = [],
                  structOrigin =
                  Just
                    Decl {
                      declInfo =
                      DeclInfo {
                        declLoc =
                        "fun_attributes.h:7:9",
                        declId = NamePair {
                          nameC = Name "FILE",
                          nameHsIdent = HsIdentifier
                            "FILE"},
                        declOrigin = NameOriginGenerated
                          (AnonId "fun_attributes.h:7:9"),
                        declAliases = [Name "FILE"],
                        declHeader = "fun_attributes.h",
                        declComment =
                        Just
                          [
                            Paragraph
                              [
                                TextContent
                                  "Attributes on functions"],
                            Paragraph
                              [
                                TextContent
                                  "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "FILE"),
                          structSizeof = 0,
                          structAlignment = 1,
                          structFields = [],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment =
                  Just
                    Comment {
                      commentTitle = Just
                        [
                          TextContent
                            "Attributes on functions"],
                      commentOrigin = Just "FILE",
                      commentChildren =
                      [
                        Paragraph
                          [
                            TextContent
                              "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}}
                (Add 0)
                (Seq [])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "FILE",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Size_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Size_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Size_t",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "fun_attributes.h:8:13",
          declId = NamePair {
            nameC = Name "size_t",
            nameHsIdent = HsIdentifier
              "Size_t"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "fun_attributes.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Size_t",
              newtypeField = HsName
                "@NsVar"
                "un_Size_t"},
            typedefType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Size_t",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9 (void) { __f1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "__f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__f1",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_8b60d38de80093fa (void) { f1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8b60d38de80093fa",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f1",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_72e7e9398b70632a (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_memalign",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_72e7e9398b70632a",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"})),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memalign",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_1e1fd866f4d88373 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_calloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_1e1fd866f4d88373",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"})),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_calloc",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_realloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer TypeVoid),
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_realloc",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_4cb3f4400795f3dc (size_t arg1) { return my_alloc1(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_alloc1",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4cb3f4400795f3dc",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc1",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45 (size_t arg1) { return my_alloc2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_alloc2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc2",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_8effe939268709e4 (signed int arg1) { return square(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8effe939268709e4",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_1dddc7f5a16104d4 (void) { return old_fn(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "old_fn",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_1dddc7f5a16104d4",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "old_fn",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "char *hs_bindgen_test_fun_attributes_77f81f76a170977e (char *arg1, char *arg2) { return my_dgettext(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_dgettext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "my_domain"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_domain",
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "my_format"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "my_format",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_77f81f76a170977e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "my_domain",
                  nameHsIdent = HsIdentifier
                    "my_domain"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "my_format",
                  nameHsIdent = HsIdentifier
                    "my_format"})
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_dgettext",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "FILE *hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d (signed int arg1, char *arg2) { return fdopen(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fdopen",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "FILE")))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              Nothing
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "FILE")
                (TypeStruct
                  NamePair {
                    nameC = Name "FILE",
                    nameHsIdent = HsIdentifier
                      "FILE"}
                  (NameOriginGenerated
                    (AnonId
                      "fun_attributes.h:7:9")))))},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fdopen",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_4a86b0420a250963 (void) { f2(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4a86b0420a250963",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_bcbe640b60445a4f (void *arg1, void *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "my_memcpy",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "dest"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "dest",
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "src"),
          functionParameterType = HsPtr
            (HsPrimType HsPrimVoid),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "src",
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "len"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_bcbe640b60445a4f",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "dest",
                  nameHsIdent = HsIdentifier
                    "dest"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "src",
                  nameHsIdent = HsIdentifier
                    "src"})
              (TypePointer TypeVoid),
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = HsIdentifier
                    "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9 (void) { fatal(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fatal",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fatal",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_71214e4420f53a0e (char *arg1) { return hash(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hash",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType = HsPtr
            (HsPrimType HsPrimCChar),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_71214e4420f53a0e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePointer
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))],
          functionAttrs =
          FunctionAttributes
            CPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentChildren = [
            Paragraph
              [
                TextContent "Marked",
                Monospace
                  [
                    Bold
                      [
                        TextContent
                          "attribute((pure))"]]]]},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void *hs_bindgen_test_fun_attributes_a71e3488215ca2b1 (size_t arg1) { return mymalloc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "mymalloc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "len"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Size_t"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "len",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_a71e3488215ca2b1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "len",
                  nameHsIdent = HsIdentifier
                    "len"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "size_t",
                    nameHsIdent = HsIdentifier
                      "Size_t"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "mymalloc",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88 (void) { foobar(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foobar",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foobar",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_ab8f0d32c1f84295 (void) { return core2_func(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "core2_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_ab8f0d32c1f84295",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "core2_func",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_f50d1e8063148c18 (void) { return sse3_func(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "sse3_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_f50d1e8063148c18",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "sse3_func",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "void hs_bindgen_test_fun_attributes_1b95ce9d55223970 (void) { f3(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_1b95ce9d55223970",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f3",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "/* get_i_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_fun_attributes_fe81510d355aff25 (void) { return &i; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "i_ptr",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsPtr (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fe81510d355aff25",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_43b222bddec511f3 (void) { return fn(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fn",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_43b222bddec511f3",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fn",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_fd90ce98862f93f3 (void) { return y(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "y",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fd90ce98862f93f3",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_8dadf866461c7be6 (void) { return x1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "x1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8dadf866461c7be6",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x1",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude
    "fun_attributes.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_31759f8ffef2c6b0 (void) { return x2(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "x2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_31759f8ffef2c6b0",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x2",
          commentChildren = []},
      foreignImportSafety = Safe}]
