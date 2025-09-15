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
      "hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9 (void) { __f1(); }",
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
        "hs_bindgen_test_fun_attributes_2e369f5f60ff28c5",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_2e369f5f60ff28c5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get___f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_2e369f5f60ff28c5 (void)) (void) { return &__f1; } ",
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
        "f1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8b60d38de80093fa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_8b60d38de80093fa (void) { f1(); }",
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
        "hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_a1b79fe9af8e18b8 (void)) (void) { return &f1; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_72e7e9398b70632a (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }",
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
        "hs_bindgen_test_fun_attributes_df18e1ec526fabcf",
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
      "hs_bindgen_test_fun_attributes_df18e1ec526fabcf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_memalign_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_df18e1ec526fabcf (void)) (size_t arg1, size_t arg2) { return &my_memalign; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_1e1fd866f4d88373 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }",
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
        "hs_bindgen_test_fun_attributes_1371a36b12f9bdfc",
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
      "hs_bindgen_test_fun_attributes_1371a36b12f9bdfc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_calloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_1371a36b12f9bdfc (void)) (size_t arg1, size_t arg2) { return &my_calloc; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }",
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
        "hs_bindgen_test_fun_attributes_149f0ffc9a274b08",
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
      "hs_bindgen_test_fun_attributes_149f0ffc9a274b08",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_realloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_149f0ffc9a274b08 (void)) (void *arg1, size_t arg2) { return &my_realloc; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_4cb3f4400795f3dc (size_t arg1) { return my_alloc1(arg1); }",
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
        "hs_bindgen_test_fun_attributes_8764901d3de7c252",
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
      "hs_bindgen_test_fun_attributes_8764901d3de7c252",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_alloc1_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_8764901d3de7c252 (void)) (size_t arg1) { return &my_alloc1; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45 (size_t arg1) { return my_alloc2(arg1); }",
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
        "hs_bindgen_test_fun_attributes_0e2a7c466f494b53",
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
      "hs_bindgen_test_fun_attributes_0e2a7c466f494b53",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_alloc2_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_0e2a7c466f494b53 (void)) (size_t arg1) { return &my_alloc2; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_8effe939268709e4 (signed int arg1) { return square(arg1); }",
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
        "hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a",
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
      "hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_631c7b52d4d4fe3a (void)) (signed int arg1) { return &square; } ",
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
        "old_fn",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_1dddc7f5a16104d4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_1dddc7f5a16104d4 (void) { return old_fn(); }",
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
        "hs_bindgen_test_fun_attributes_407cc567cd7ef4a1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_407cc567cd7ef4a1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_old_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_407cc567cd7ef4a1 (void)) (void) { return &old_fn; } ",
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
      "hs_bindgen_test_fun_attributes_77f81f76a170977e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char *hs_bindgen_test_fun_attributes_77f81f76a170977e (char *arg1, char const *arg2) { return my_dgettext(arg1, arg2); }",
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
        "hs_bindgen_test_fun_attributes_60702a9764046d9e",
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
      "hs_bindgen_test_fun_attributes_60702a9764046d9e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_dgettext_ptr */ __attribute__ ((const)) char *(*hs_bindgen_test_fun_attributes_60702a9764046d9e (void)) (char *arg1, char const *arg2) { return &my_dgettext; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "FILE *hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d (signed int arg1, char const *arg2) { return fdopen(arg1, arg2); }",
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
        "hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4",
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
      "hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fdopen_ptr */ __attribute__ ((const)) FILE *(*hs_bindgen_test_fun_attributes_e8eae9d0dd40ede4 (void)) (signed int arg1, char const *arg2) { return &fdopen; } ",
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
        "f2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4a86b0420a250963",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_4a86b0420a250963 (void) { f2(); }",
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
        "hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_74cfd16f2b7e27ba (void)) (void) { return &f2; } ",
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
      "hs_bindgen_test_fun_attributes_bcbe640b60445a4f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_bcbe640b60445a4f (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }",
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
        "hs_bindgen_test_fun_attributes_de9d3228e8bac25c",
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
      "hs_bindgen_test_fun_attributes_de9d3228e8bac25c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_de9d3228e8bac25c (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } ",
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
        "fatal",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9 (void) { fatal(); }",
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
        "hs_bindgen_test_fun_attributes_4a9c795c1867222e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_4a9c795c1867222e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fatal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_4a9c795c1867222e (void)) (void) { return &fatal; } ",
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_71214e4420f53a0e (char *arg1) { return hash(arg1); }",
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
        "hs_bindgen_test_fun_attributes_241bedb74b8016f3",
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
      "hs_bindgen_test_fun_attributes_241bedb74b8016f3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_241bedb74b8016f3 (void)) (char *arg1) { return &hash; } ",
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
      "hs_bindgen_test_fun_attributes_a71e3488215ca2b1",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void *hs_bindgen_test_fun_attributes_a71e3488215ca2b1 (size_t arg1) { return mymalloc(arg1); }",
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
        "hs_bindgen_test_fun_attributes_adfbe41965d544a3",
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
      "hs_bindgen_test_fun_attributes_adfbe41965d544a3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_mymalloc_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_fun_attributes_adfbe41965d544a3 (void)) (size_t arg1) { return &mymalloc; } ",
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
        "foobar",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88 (void) { foobar(); }",
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
        "hs_bindgen_test_fun_attributes_730ef6ad01273b1d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_730ef6ad01273b1d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foobar_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_730ef6ad01273b1d (void)) (void) { return &foobar; } ",
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
        "core2_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_ab8f0d32c1f84295",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_ab8f0d32c1f84295 (void) { return core2_func(); }",
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
        "hs_bindgen_test_fun_attributes_abb5c394ed250f25",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_abb5c394ed250f25",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_core2_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_abb5c394ed250f25 (void)) (void) { return &core2_func; } ",
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
        "sse3_func",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_f50d1e8063148c18",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_f50d1e8063148c18 (void) { return sse3_func(); }",
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
        "hs_bindgen_test_fun_attributes_12616d0501d14a7a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_12616d0501d14a7a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_sse3_func_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_12616d0501d14a7a (void)) (void) { return &sse3_func; } ",
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
        "f3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_1b95ce9d55223970",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_fun_attributes_1b95ce9d55223970 (void) { f3(); }",
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
        "hs_bindgen_test_fun_attributes_08809dca6bfda237",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_08809dca6bfda237",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_fun_attributes_08809dca6bfda237 (void)) (void) { return &f3; } ",
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
        "hs_bindgen_test_fun_attributes_fe81510d355aff25",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fe81510d355aff25",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_i_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_fun_attributes_fe81510d355aff25 (void) { return &i; } ",
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
  DeclSimple,
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_43b222bddec511f3 (void) { return fn(); }",
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
        "hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fn_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_9d0d1087d0fa4b10 (void)) (void) { return &fn; } ",
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
        "y",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_fd90ce98862f93f3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_fd90ce98862f93f3 (void) { return y(); }",
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
        "hs_bindgen_test_fun_attributes_2d9291944d890d18",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_2d9291944d890d18",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_y_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_2d9291944d890d18 (void)) (void) { return &y; } ",
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
        "x1",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8dadf866461c7be6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_8dadf866461c7be6 (void) { return x1(); }",
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
        "hs_bindgen_test_fun_attributes_8839814efdc23f88",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_8839814efdc23f88",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_x1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_8839814efdc23f88 (void)) (void) { return &x1; } ",
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
        "x2",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_31759f8ffef2c6b0",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_fun_attributes_31759f8ffef2c6b0 (void) { return x2(); }",
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
        "hs_bindgen_test_fun_attributes_c63d8c58f9a27a01",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_c63d8c58f9a27a01",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_x2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_c63d8c58f9a27a01 (void)) (void) { return &x2; } ",
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
  DeclSimple]
