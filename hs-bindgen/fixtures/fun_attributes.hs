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
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["fun_attributes.h"],
                headerInclude =
                "fun_attributes.h"},
            declComment =
            Just
              (Comment
                [
                  Paragraph
                    [
                      TextContent
                        "Attributes on functions"],
                  Paragraph
                    [
                      TextContent
                        "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
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
          commentLocation = Just
            "fun_attributes.h:7:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
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
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["fun_attributes.h"],
                    headerInclude =
                    "fun_attributes.h"},
                declComment =
                Just
                  (Comment
                    [
                      Paragraph
                        [
                          TextContent
                            "Attributes on functions"],
                      Paragraph
                        [
                          TextContent
                            "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
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
              commentLocation = Just
                "fun_attributes.h:7:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["fun_attributes.h"],
                  headerInclude =
                  "fun_attributes.h"},
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
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["fun_attributes.h"],
                            headerInclude =
                            "fun_attributes.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "Attributes on functions"],
                              Paragraph
                                [
                                  TextContent
                                    "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
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
                      commentLocation = Just
                        "fun_attributes.h:7:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["fun_attributes.h"],
                          headerInclude =
                          "fun_attributes.h"},
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
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["fun_attributes.h"],
                            headerInclude =
                            "fun_attributes.h"},
                        declComment =
                        Just
                          (Comment
                            [
                              Paragraph
                                [
                                  TextContent
                                    "Attributes on functions"],
                              Paragraph
                                [
                                  TextContent
                                    "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]])},
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
                      commentLocation = Just
                        "fun_attributes.h:7:9",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["fun_attributes.h"],
                          headerInclude =
                          "fun_attributes.h"},
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
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
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "size_t",
          commentLocation = Just
            "fun_attributes.h:8:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []}},
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
      "hs_bindgen_test_fun_attributes_8de545512324157b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_8de545512324157b (void) { __f1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:16:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_a2f84d2570ef3892",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_a2f84d2570ef3892 (void) { f1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_cefda6b95395d829",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_cefda6b95395d829 (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:21:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_e25f06c3ebec2536",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_e25f06c3ebec2536 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:26:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_51fa664668350a00",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_51fa664668350a00 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:27:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_93a5d6b7d4e02c33",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_93a5d6b7d4e02c33 (size_t arg1) { return my_alloc1(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:32:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_c948fd867be322fa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_c948fd867be322fa (size_t arg1) { return my_alloc2(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:33:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_55e5eb89e54abf83",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_55e5eb89e54abf83 (signed int arg1) { return square(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:37:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_1040c24c74db8069",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_1040c24c74db8069 (void) { return old_fn(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:46:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_023f7813e909f518",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char *hs_bindgen_test_fun_attributes_023f7813e909f518 (char *arg1, char const *arg2) { return my_dgettext(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
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
          commentLocation = Just
            "fun_attributes.h:57:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_e39bbd59f1c96c14",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "FILE *hs_bindgen_test_fun_attributes_e39bbd59f1c96c14 (signed int arg1, char const *arg2) { return fdopen(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
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
          commentLocation = Just
            "fun_attributes.h:68:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_1d043de05a457e90",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_1d043de05a457e90 (void) { f2(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:72:65",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4b3bfd2d72a2db5d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_4b3bfd2d72a2db5d (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
              (TypePointer
                (TypeConst TypeVoid)),
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
          commentLocation = Just
            "fun_attributes.h:78:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_348fe595d62421cf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_348fe595d62421cf (void) { fatal(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:95:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_e30754e2591f701a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_e30754e2591f701a (char *arg1) { return hash(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:103:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_f6f68a022a15937a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_f6f68a022a15937a (size_t arg1) { return mymalloc(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:108:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_d1bf41da7ab64db1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_d1bf41da7ab64db1 (void) { foobar(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:112:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_00405e83bcb9b271",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_00405e83bcb9b271 (void) { return core2_func(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:119:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_06e7d2f8bcf43684",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_06e7d2f8bcf43684 (void) { return sse3_func(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:120:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_e23eff1955ebb459",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_e23eff1955ebb459 (void) { f3(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:124:49",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_ef0eea5f61ef9228",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_ef0eea5f61ef9228 (void) { return fn(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:129:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_b007466f7ff1cf28",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_b007466f7ff1cf28 (void) { return y(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:135:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_8c9825e1b20a7ea1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_8c9825e1b20a7ea1 (void) { return x1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:138:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_c80d61b7727dab77",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_c80d61b7727dab77 (void) { return x2(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:141:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      "hs_bindgen_test_fun_attributes_b44da51a357ae983",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_b44da51a357ae983 (void) { __f1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:16:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_c1788128a5b1c813",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_c1788128a5b1c813 (void) { f1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_9ca07f6722bd48dc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_9ca07f6722bd48dc (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:21:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_72df124450cc6d26",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_72df124450cc6d26 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:26:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_effc1fd567613950",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_effc1fd567613950 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:27:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_b3544e53af074ef1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_b3544e53af074ef1 (size_t arg1) { return my_alloc1(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:32:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_0b659f90fec40284",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_0b659f90fec40284 (size_t arg1) { return my_alloc2(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:33:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_cb3c687f16289bb3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_cb3c687f16289bb3 (signed int arg1) { return square(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:37:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_febe1b1c3f69ce2f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_febe1b1c3f69ce2f (void) { return old_fn(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:46:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_d492bd76e82890da",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char *hs_bindgen_test_fun_attributes_d492bd76e82890da (char *arg1, char const *arg2) { return my_dgettext(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
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
          commentLocation = Just
            "fun_attributes.h:57:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_3c91a267bd66cc10",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "FILE *hs_bindgen_test_fun_attributes_3c91a267bd66cc10 (signed int arg1, char const *arg2) { return fdopen(arg1, arg2); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))))))],
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
          commentLocation = Just
            "fun_attributes.h:68:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_14361e995fb5684a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_14361e995fb5684a (void) { f2(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:72:65",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_e8c4a96cefd6117e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_e8c4a96cefd6117e (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
              (TypePointer
                (TypeConst TypeVoid)),
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
          commentLocation = Just
            "fun_attributes.h:78:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_64aa41e835dbb892",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_64aa41e835dbb892 (void) { fatal(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:95:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_88887d4b5f42f079",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_88887d4b5f42f079 (char *arg1) { return hash(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:103:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
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
      foreignImportSafety = Unsafe},
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimVoid))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_31e6e14ecb251fa2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_31e6e14ecb251fa2 (size_t arg1) { return mymalloc(arg1); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:108:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_bb77a71513994934",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_bb77a71513994934 (void) { foobar(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:112:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_640ec5b51b0819d1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_640ec5b51b0819d1 (void) { return core2_func(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:119:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_a1f7636643d63586",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_a1f7636643d63586 (void) { return sse3_func(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:120:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_2bef032cbe15ffd0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_2bef032cbe15ffd0 (void) { f3(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:124:49",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_8f406104a21ff66e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_8f406104a21ff66e (void) { return fn(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:129:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_4beb0cbf65b462bd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_4beb0cbf65b462bd (void) { return y(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:135:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_ac7386c785058f4d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_ac7386c785058f4d (void) { return x1(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:138:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
      "hs_bindgen_test_fun_attributes_b6f428ed915f03cc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_b6f428ed915f03cc (void) { return x2(); }",
          capiWrapperImport =
          "fun_attributes.h"},
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
          commentLocation = Just
            "fun_attributes.h:141:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_7003b306f73c174b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_7003b306f73c174b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get___f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_7003b306f73c174b (void)) (void) { return &__f1; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "__f1",
          commentLocation = Just
            "fun_attributes.h:16:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_5469bdc0395f86c1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_5469bdc0395f86c1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_5469bdc0395f86c1 (void)) (void) { return &f1; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f1",
          commentLocation = Just
            "fun_attributes.h:17:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_b3c956e53724162c",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Size_t"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_b3c956e53724162c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_memalign_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_b3c956e53724162c (void)) (size_t arg1, size_t arg2) { return &my_memalign; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memalign",
          commentLocation = Just
            "fun_attributes.h:21:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_733646ca96f39979",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Size_t"))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_733646ca96f39979",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_calloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_733646ca96f39979 (void)) (size_t arg1, size_t arg2) { return &my_calloc; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"}),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_calloc",
          commentLocation = Just
            "fun_attributes.h:26:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_94e8271f186110fd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsFun
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Size_t"))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimVoid))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_94e8271f186110fd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_realloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_94e8271f186110fd (void)) (void *arg1, size_t arg2) { return &my_realloc; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer TypeVoid,
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_realloc",
          commentLocation = Just
            "fun_attributes.h:27:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_48d9862d70f58e70",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_48d9862d70f58e70",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_alloc1_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_48d9862d70f58e70 (void)) (size_t arg1) { return &my_alloc1; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc1",
          commentLocation = Just
            "fun_attributes.h:32:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_17a11fd10dc57357",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_17a11fd10dc57357",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_alloc2_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_17a11fd10dc57357 (void)) (size_t arg1) { return &my_alloc2; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_alloc2",
          commentLocation = Just
            "fun_attributes.h:33:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_c41111f40a04cdc9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_c41111f40a04cdc9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_c41111f40a04cdc9 (void)) (signed int arg1) { return &square; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "square",
          commentLocation = Just
            "fun_attributes.h:37:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_3add0261fa83e1dd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_3add0261fa83e1dd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_old_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_3add0261fa83e1dd (void)) (void) { return &old_fn; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "old_fn",
          commentLocation = Just
            "fun_attributes.h:46:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_a0be4f488601c252",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsFun
                (HsPtr (HsPrimType HsPrimCChar))
                (HsIO
                  (HsPtr
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_a0be4f488601c252",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_dgettext_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_fun_attributes_a0be4f488601c252 (void)) (char *arg1, char const *arg2) { return &my_dgettext; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))),
            TypePointer
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
          (TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_dgettext",
          commentLocation = Just
            "fun_attributes.h:57:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_2b987c3b5c01a326",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPtr (HsPrimType HsPrimCChar))
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "FILE")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_2b987c3b5c01a326",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fdopen_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_test_fun_attributes_2b987c3b5c01a326 (void)) (signed int arg1, char const *arg2) { return &fdopen; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePointer
              (TypeConst
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed)))))]
          (TypePointer
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
                      "fun_attributes.h:7:9"))))))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fdopen",
          commentLocation = Just
            "fun_attributes.h:68:9",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_490ca7e8c8282a69",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_490ca7e8c8282a69",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_490ca7e8c8282a69 (void)) (void) { return &f2; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f2",
          commentLocation = Just
            "fun_attributes.h:72:65",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimVoid))
              (HsFun
                (HsPtr (HsPrimType HsPrimVoid))
                (HsFun
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Size_t"))
                  (HsIO
                    (HsPtr
                      (HsPrimType HsPrimVoid)))))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_e2e8b5d5ac435de8 (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer TypeVoid,
            TypePointer
              (TypeConst TypeVoid),
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "my_memcpy",
          commentLocation = Just
            "fun_attributes.h:78:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fatal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_ea0bb781f9eca7f5 (void)) (void) { return &fatal; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fatal",
          commentLocation = Just
            "fun_attributes.h:95:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_4de9606eb9c5dd01",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr (HsPrimType HsPrimCChar))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4de9606eb9c5dd01",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_4de9606eb9c5dd01 (void)) (char *arg1) { return &hash; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))))]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "hash",
          commentLocation = Just
            "fun_attributes.h:103:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_4ce141c884649d49",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Size_t"))
              (HsIO
                (HsPtr
                  (HsPrimType HsPrimVoid)))))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4ce141c884649d49",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_mymalloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_4ce141c884649d49 (void)) (size_t arg1) { return &mymalloc; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "size_t",
                  nameHsIdent = HsIdentifier
                    "Size_t"})]
          (TypePointer TypeVoid)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "mymalloc",
          commentLocation = Just
            "fun_attributes.h:108:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_5c243ced544ab0aa",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_5c243ced544ab0aa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foobar_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_5c243ced544ab0aa (void)) (void) { return &foobar; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foobar",
          commentLocation = Just
            "fun_attributes.h:112:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_14ef55245a14f816",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_14ef55245a14f816",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_core2_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_14ef55245a14f816 (void)) (void) { return &core2_func; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "core2_func",
          commentLocation = Just
            "fun_attributes.h:119:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_72956748bb6eee67",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_72956748bb6eee67",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_sse3_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_72956748bb6eee67 (void)) (void) { return &sse3_func; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "sse3_func",
          commentLocation = Just
            "fun_attributes.h:120:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_38506a9ac5626bf2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_38506a9ac5626bf2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_38506a9ac5626bf2 (void)) (void) { return &f3; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "f3",
          commentLocation = Just
            "fun_attributes.h:124:49",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_5929da82079150d1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_5929da82079150d1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_5929da82079150d1 (void)) (void) { return &fn; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fn",
          commentLocation = Just
            "fun_attributes.h:129:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_y_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_7bcb4a1873e6ece6 (void)) (void) { return &y; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "y",
          commentLocation = Just
            "fun_attributes.h:135:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_11098262b345351a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_11098262b345351a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_x1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_11098262b345351a (void)) (void) { return &x1; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x1",
          commentLocation = Just
            "fun_attributes.h:138:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_0d19f83087f278f9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_0d19f83087f278f9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_x2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_0d19f83087f278f9 (void)) (void) { return &x2; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypeFun
          []
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "x2",
          commentLocation = Just
            "fun_attributes.h:141:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_i_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_fun_attributes_cdc30ae5fb72cd6e (void) { return &i; } ",
          capiWrapperImport =
          "fun_attributes.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "i",
          commentLocation = Just
            "fun_attributes.h:125:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["fun_attributes.h"],
              headerInclude =
              "fun_attributes.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
